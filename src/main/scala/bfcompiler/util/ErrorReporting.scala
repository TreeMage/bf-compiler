package bfcompiler.util

import bfcompiler.intermediate.IntermediateCompilationError
import bfcompiler.lexer.LexerError

object ErrorReporting {

  private def reportErrorsGeneric[A](errors: List[A])(formatter: A => String): Unit =
    errors.zipWithIndex.foreach {
      case (error, index) =>
        val adjustedIndex = index + 1
        val errorMessage = s"$adjustedIndex. ${formatter(error)}"
        println(errorMessage)
    }

  def reportLexerErrors(errors: List[LexerError]): Unit =
    reportErrorsGeneric(errors) {
      case LexerError.IOError(path, cause) => s"Lexing of file $path failed: File not found."
      case LexerError.InvalidToken(value, location) => s"${location.toStringLocation} Encountered invalid token '$value'."
    }


  def reportIntermediateCompilationErrors(errors: List[IntermediateCompilationError]): Unit =
    reportErrorsGeneric(errors) {
      case IntermediateCompilationError.UnmatchedStartLoop(location) =>
        s"${location.toStringLocation} Encountered unmatched start of loop."
      case IntermediateCompilationError.UnmatchedEndLoop(location) =>
        s"${location.toStringLocation} Encountered unmatched end of loop."
    }
}
