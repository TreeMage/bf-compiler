import bfcompiler.cli.CliApp
import bfcompiler.intermediate.{IntermediateCompilationError, IntermediateCompiler}
import bfcompiler.interpreter.Interpreter
import bfcompiler.lexer.{Lexer, LexerError}
import bfcompiler.util.ErrorReporting

import java.nio.file.Paths

@main def main(args: String*): Unit =
  CliApp.run(args, sys.env)
