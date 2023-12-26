package bfcompiler.cli

import bfcompiler.intermediate.IntermediateCompiler
import bfcompiler.lexer.Lexer
import bfcompiler.native.{
  AssemblerError,
  ExternalAssemblerWrapper,
  ExternalLinkerWrapper,
  LdConfig,
  LinkerError,
  NativeCompiler
}
import bfcompiler.util.ErrorReporting
import cats.data.Validated
import com.monovore.decline.{Command, Opts}
import cats.implicits.*

import java.io.PrintWriter
import java.nio.file.Path
import scala.language.postfixOps
import scala.util.Using
import scala.sys.process.*

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
        Lexer.default.lexFile(config.sourcePath) match
          case Validated.Invalid(errors) =>
            ErrorReporting.reportLexerErrors(errors)
            sys.exit(1)
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
                  case _ => ()
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
