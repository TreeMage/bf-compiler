package bfcompiler.cli

import bfcompiler.intermediate.IntermediateCompiler
import bfcompiler.lexer.Lexer
import bfcompiler.native.*
import bfcompiler.util.{ErrorReporting, FileIO, Logging}
import cats.data.Validated
import cats.implicits.*
import com.monovore.decline.{Command, Opts}

import java.nio.file.Path
import scala.language.postfixOps
import scala.sys.process.Process

case class CompileConfig(
    sourcePath: Path,
    outputPath: Path,
    run: Boolean,
    debug: Boolean,
    assemblerCommand: String
)

object CompileCommand:
  type NativeCompilationError = AssemblerError | LinkerError
  lazy val command: Opts[Unit] = Opts.subcommand(
    Command(
      name = "compile",
      header = "Compiles the the given file to native code."
    )(
      options.map(config =>
        FileIO.readAllLines(config.sourcePath) match
          case Left(error) =>
            ErrorReporting.reportIOError(error)
            sys.exit(1)
          case Right(lines) =>
            Lexer.default.lex(lines) match
              case Validated.Invalid(errors) =>
                ErrorReporting.reportLexerErrors(errors)
              case Validated.Valid(tokens) =>
                IntermediateCompiler.default.compile(tokens) match
                  case Validated.Invalid(errors) =>
                    ErrorReporting.reportIntermediateCompilationErrors(errors)
                    sys.exit(1)
                  case Validated.Valid(program) =>
                    if (config.debug) println(program.asTree)
                    val inputFileNameWithoutExtension =
                      config.sourcePath.getFileName.toString.split("\\.", 2)(0)
                    val outputDirectory = config.outputPath
                    outputDirectory.toFile.mkdir()
                    val executableFilePath =
                      outputDirectory.resolve(s"$inputFileNameWithoutExtension")
                    val assembly = NativeCompiler.arm.compile(program)
                    val compilationResult =
                      for
                        assemblerResult <- ExternalAssemblerWrapper.arm64macos
                          .compile(
                            assembly,
                            outputDirectory,
                            inputFileNameWithoutExtension
                          )
                          .leftWiden[NativeCompilationError]
                        _ <- ExternalLinkerWrapper
                          .ld(LdConfig.arm64macos)
                          .compile(
                            assemblerResult.objectFilePath,
                            executableFilePath,
                            "_start"
                          )
                      yield ()
                    compilationResult match
                      case Left(error) =>
                        ErrorReporting.reportNativeCompilationError(error)
                      case _ =>
                        if (config.run)
                          val command = executableFilePath.toString
                          Logging.commandExecution(command)
                          val exitCode = Process(executableFilePath.toString).!
                          println(
                            s"[INFO] Process finished with exit code $exitCode"
                          )
      )
    )
  )
  private val options: Opts[CompileConfig] = (
    Opts.argument[Path](metavar = "source file").map(_.toAbsolutePath),
    Opts.argument[Path](metavar = "output file").map(_.toAbsolutePath),
    Opts.flag("run", help = "Run the generated executable").orFalse,
    Opts.flag("debug", help = "Enable debug output").orFalse,
    Opts.option[String]("asm", help = "Assembler executable").withDefault("as")
  ).mapN { case (sourcePath, outputPath, run, debug, assembler) =>
    CompileConfig(sourcePath, outputPath, run, debug, assembler)
  }
