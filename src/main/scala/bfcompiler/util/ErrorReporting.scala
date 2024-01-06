package bfcompiler.util

import bfcompiler.cli.CompileCommand.NativeCompilationError
import bfcompiler.intermediate.IntermediateCompilationError
import bfcompiler.interpreter.InterpreterError
import bfcompiler.lexer.LexerError
import bfcompiler.native.{AssemblerError, LinkerError}
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
        s"${location.asString} Encountered invalid token '$value'."
    }

  def reportIntermediateCompilationErrors(
      errors: NonEmptyList[IntermediateCompilationError]
  ): Unit =
    reportErrorsGeneric(
      errors,
      "Intermediate code generation failed due to the following issue(s):"
    ) {
      case IntermediateCompilationError.UnmatchedStartLoop(location) =>
        s"${location.asString} Encountered unmatched start of loop."
      case IntermediateCompilationError.UnmatchedEndLoop(location) =>
        s"${location.asString} Encountered unmatched end of loop."
    }

  def reportInterpretationError(error: InterpreterError): Unit =
    reportErrorsGeneric(
      NonEmptyList.one(error),
      "Interpretation of the program failed due to the following issue:"
    ) { case InterpreterError.DataPointerOutOfBounds(index, cause) =>
      s"""Data pointer is out of bounds (pointing at index $index). The offending instruction is ${cause.op} at ${cause.location.asString}"""
    }

  def reportNativeCompilationError(error: NativeCompilationError): Unit =
    error match
      case assemblerError: AssemblerError =>
        reportAssemblerError(assemblerError)
      case linkerError: LinkerError =>
        reportLinkerError(linkerError)

  private def reportAssemblerError(error: AssemblerError): Unit =
    reportErrorsGeneric(
      NonEmptyList.one(error),
      "Assembly of the program failed due to the following issue:"
    ) {
      case AssemblerError.IOError(cause) =>
        s"An error occurred while writing the genreated assembly to disk.\n $cause"
      case AssemblerError.AssemblingFailed(cause) =>
        s"The program could not be assembled due to the following assembler error:\n $cause"
    }

  private def reportLinkerError(error: LinkerError): Unit =
    reportErrorsGeneric(
      NonEmptyList.one(error),
      "Linking of the program failed due to the following issue:"
    ) { case LinkerError.LinkingFailed(cause) =>
      cause.toString
    }

  private def reportErrorsGeneric[A](
      errors: NonEmptyList[A],
      header: String = ""
  )(
      formatter: A => String
  ): Unit =
    if (!header.isBlank)
      Console.err.println(header)
    val messages = errors.zipWithIndex
      .foldLeft(List.empty[String]) { case (acc, (error, index)) =>
        val adjustedIndex = index + 1
        val errorMessage  = s"\t$adjustedIndex. ${formatter(error)}"
        errorMessage +: acc
      }
      .reverse
    Console.err.println(messages.mkString("\n"))

  def reportIOError(error: Throwable): Unit =
    Console.err.println(s"An IO error occurred: $error")
}
