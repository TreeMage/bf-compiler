package bfcompiler.util

import bfcompiler.intermediate.IntermediateCompilationError
import bfcompiler.interpreter.InterpreterError
import bfcompiler.lexer.LexerError

import cats.data.NonEmptyList

object ErrorReporting {

  def reportLexerErrors(errors: NonEmptyList[LexerError]): Unit =
    reportErrorsGeneric(
      errors,
      "Lexing failed due to the following issue(s):"
    ) {
      case LexerError.IOError(path, cause) =>
        s"Lexing of file $path failed: File not found."
      case LexerError.InvalidToken(value, location) =>
        s"${location.toStringLocation} Encountered invalid token '$value'."
    }

  private def reportErrorsGeneric[A](
      errors: NonEmptyList[A],
      header: String = ""
  )(
      formatter: A => String
  ): Unit =
    if (!header.isBlank)
      Console.err.println(header)
    val messages = errors.zipWithIndex.foldLeft(List.empty[String]) { case (acc,(error, index)) =>
      val adjustedIndex = index + 1
      val errorMessage  = s"\t$adjustedIndex. ${formatter(error)}"
      errorMessage +: acc
    }.reverse
    Console.err.println(messages.mkString("\n"))

  def reportIntermediateCompilationErrors(
      errors: NonEmptyList[IntermediateCompilationError]
  ): Unit =
    reportErrorsGeneric(
      errors,
      "Intermediate code generation failed due to the following issue(s):"
    ) {
      case IntermediateCompilationError.UnmatchedStartLoop(location) =>
        s"${location.toStringLocation} Encountered unmatched start of loop."
      case IntermediateCompilationError.UnmatchedEndLoop(location) =>
        s"${location.toStringLocation} Encountered unmatched end of loop."
    }

  def reportInterpretationError(error: InterpreterError): Unit =
    reportErrorsGeneric(NonEmptyList.one(error), "Interpretation of the program failed") {
      case InterpreterError.DataPointerOutOfBounds(index, cause) =>
        s"""Data pointer is out of bounds (pointing at index $index). The offending instruction is ${cause.op} at ${cause.location.toStringLocation}"""
    }
}
