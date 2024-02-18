import bfcompiler.common.{Location, Program, Token}
import bfcompiler.intermediate.{
  IntermediateCompilationError,
  IntermediateCompiler,
  Operation,
  OperationType
}
import bfcompiler.lexer.Lexeme
import cats.data.{NonEmptyList, Validated}

class IntermediateCompilerSuite extends munit.FunSuite:
  private val compiler      = IntermediateCompiler.default
  private val dummyLocation = Location.Local(4, 7)

  test("Intermediate compiler generates correct instructions") {
    assertEquals(
      compiler.compile(
        List(
          Token(Lexeme.IncrementDataPointer, dummyLocation),
          Token(Lexeme.DecrementDataPointer, dummyLocation),
          Token(Lexeme.Increment, dummyLocation),
          Token(Lexeme.Decrement, dummyLocation),
          Token(Lexeme.Read, dummyLocation),
          Token(Lexeme.Write, dummyLocation),
          Token(Lexeme.JumpForwardEqualZero, dummyLocation),
          Token(Lexeme.JumpBackwardNotEqualZero, dummyLocation)
        )
      ),
      Validated.valid(
        Program(
          List(
            Operation(OperationType.IncrementDataPointer(1), dummyLocation),
            Operation(OperationType.DecrementDataPointer(1), dummyLocation),
            Operation(OperationType.Increment(1), dummyLocation),
            Operation(OperationType.Decrement(1), dummyLocation),
            Operation(OperationType.Read, dummyLocation),
            Operation(OperationType.Write, dummyLocation),
            Operation(OperationType.JumpForwardEqualZero(7), dummyLocation),
            Operation(OperationType.JumpBackwardNotEqualZero(6), dummyLocation)
          )
        )
      )
    )
  }

  test("Intermediate compiler resolves non-nested loops correctly") {
    assertEquals(
      compiler.compile(
        List(
          Token(Lexeme.JumpForwardEqualZero, dummyLocation),
          Token(Lexeme.Increment, dummyLocation),
          Token(Lexeme.JumpBackwardNotEqualZero, dummyLocation)
        )
      ),
      Validated.valid(
        Program(
          List(
            Operation(OperationType.JumpForwardEqualZero(2), dummyLocation),
            Operation(OperationType.Increment(1), dummyLocation),
            Operation(OperationType.JumpBackwardNotEqualZero(0), dummyLocation)
          )
        )
      )
    )
  }

  test("Intermediate compiler resolves nested loops correctly") {
    assertEquals(
      compiler.compile(
        List(
          Token(Lexeme.JumpForwardEqualZero, dummyLocation),
          Token(Lexeme.Increment, dummyLocation),
          Token(Lexeme.JumpForwardEqualZero, dummyLocation),
          Token(Lexeme.Increment, dummyLocation),
          Token(Lexeme.JumpBackwardNotEqualZero, dummyLocation),
          Token(Lexeme.Increment, dummyLocation),
          Token(Lexeme.JumpBackwardNotEqualZero, dummyLocation)
        )
      ),
      Validated.valid(
        Program(
          List(
            Operation(OperationType.JumpForwardEqualZero(6), dummyLocation),
            Operation(OperationType.Increment(1), dummyLocation),
            Operation(OperationType.JumpForwardEqualZero(4), dummyLocation),
            Operation(OperationType.Increment(1), dummyLocation),
            Operation(OperationType.JumpBackwardNotEqualZero(2), dummyLocation),
            Operation(OperationType.Increment(1), dummyLocation),
            Operation(OperationType.JumpBackwardNotEqualZero(0), dummyLocation)
          )
        )
      )
    )
  }

  test("Intermediate compiler reports unmatched start loop correctly") {
    assertEquals(
      compiler.compile(
        List(
          Token(Lexeme.JumpForwardEqualZero, dummyLocation)
        )
      ),
      Validated.invalid(
        NonEmptyList.of(
          IntermediateCompilationError.UnmatchedStartLoop(dummyLocation)
        )
      )
    )
  }

  test("Intermediate compiler reports unmatched end loop correctly") {
    assertEquals(
      compiler.compile(
        List(
          Token(Lexeme.JumpBackwardNotEqualZero, dummyLocation)
        )
      ),
      Validated.invalid(
        NonEmptyList.of(
          IntermediateCompilationError.UnmatchedEndLoop(dummyLocation)
        )
      )
    )
  }
