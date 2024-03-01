package bfcompiler.cli

import bfcompiler.intermediate.IntermediateCompiler
import bfcompiler.interpreter.Interpreter
import bfcompiler.lexer.Lexer
import bfcompiler.optimizer.Optimizer
import bfcompiler.util.{ErrorReporting, FileIO}
import cats.data.Validated
import cats.implicits.*
import com.monovore.decline.{Command, Opts}

import java.nio.file.Path

case class InterpretConfig(sourcePath: Path, debug: Boolean, optimize: Boolean)

object InterpretCommand:
  lazy val command: Opts[Unit] = Opts.subcommand(
    Command(
      name = "interpret",
      header = "Interprets the given file."
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
                sys.exit(1)
              case Validated.Valid(tokens) =>
                IntermediateCompiler.default.compile(tokens) match
                  case Validated.Invalid(errors) =>
                    ErrorReporting.reportIntermediateCompilationErrors(errors)
                    sys.exit(1)
                  case Validated.Valid(program) =>
                    if (config.debug) println(program.asTree)
                    val programToInterpret =
                      if (config.optimize)
                        Optimizer.default.optimize(program)
                      else program
                    Interpreter.default.run(programToInterpret) match
                      case Left(error) =>
                        ErrorReporting.reportInterpretationError(error)
                        sys.exit(1)
                      case Right(_) => ()
      )
    )
  )
  private val options: Opts[InterpretConfig] = (
    Opts.argument[Path](metavar = "source file").map(_.toAbsolutePath),
    Opts.flag(long = "debug", help = "Enable debug output").orFalse,
    Opts.flag(long = "optimize", help = "Enable optimization").orFalse
  ).mapN { case (path, debug, optimize) =>
    InterpretConfig(path, debug, optimize)
  }
