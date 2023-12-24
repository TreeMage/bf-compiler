import bfcompiler.intermediate.{IntermediateCompilationError, IntermediateCompiler}
import bfcompiler.interpreter.Interpreter
import bfcompiler.lexer.{Lexer, LexerError}
import bfcompiler.util.ErrorReporting

import java.nio.file.Paths

@main def main(): Unit =
  val debug = true
  val path = Paths.get("examples/03-addition.bf").toAbsolutePath
  Lexer.default.lexFile(path) match
    case Left(errors) => ErrorReporting.reportLexerErrors(errors)
    case Right(tokens) =>
      IntermediateCompiler.default.compile(tokens) match
        case Left(errors) => ErrorReporting.reportIntermediateCompilationErrors(errors)
        case Right(program) =>
          if (debug) println(program.asTree)
          Interpreter.default.run(program)
