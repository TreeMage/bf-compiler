package bfcompiler.cli

import bfcompiler.intermediate.IntermediateCompiler
import bfcompiler.interpreter.Interpreter
import bfcompiler.lexer.Lexer
import bfcompiler.util.ErrorReporting
import com.monovore.decline.{Command, Opts}
import cats.implicits.*

import java.nio.file.Path

case class InterpretConfig(sourcePath: Path, debug: Boolean)

object InterpretCommand:
  private val interpretOptions: Opts[InterpretConfig] = (
      Opts.argument[Path](metavar = "source file").map(_.toAbsolutePath),
      Opts.flag(long = "debug", help = "Enable debug output").orFalse
    ).mapN {
      case (path, debug) => InterpretConfig(path, debug)
  }

  val command: Opts[Unit] = Opts.subcommand(Command(
    name = "interpret",
    header = "Interprets the given file."
  )(interpretOptions.map(config =>
    Lexer.default.lexFile(config.sourcePath) match
      case Left(errors) => ErrorReporting.reportLexerErrors(errors)
      case Right(tokens) =>
        IntermediateCompiler.default.compile(tokens) match
          case Left(errors) => ErrorReporting.reportIntermediateCompilationErrors(errors)
          case Right(program) =>
            if (config.debug) println(program.asTree)
            Interpreter.default.run(program)
  )))



