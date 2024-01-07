import bfcompiler.common.Location.Local
import bfcompiler.common.{Location, Token}
import bfcompiler.lexer.Lexeme.Increment
import bfcompiler.lexer.{Lexeme, Lexer, LexerError}
import cats.data.{NonEmptyList, Validated}

class LexerSuite extends munit.FunSuite:
  private val lexer = Lexer.default

  private val singleTokenLocation = Location.Local(0, 0)

  test("Lexer produces correct tokens and locations in single line input") {
    assertEquals(
      lexer.lex(List("><+-,.[]")),
      Validated.valid(
        List(
          Lexeme.IncrementDataPointer,
          Lexeme.DecrementDataPointer,
          Lexeme.Increment,
          Lexeme.Decrement,
          Lexeme.Read,
          Lexeme.Write,
          Lexeme.JumpForwardEqualZero,
          Lexeme.JumpBackwardNotEqualZero
        ).zipWithIndex.map { case (lexeme, column) =>
          Token(lexeme, Location.Local(0, column))
        }
      )
    )
  }

  test("Lexer produces correct tokens and locations in multiline input") {
    assertEquals(
      lexer.lex(List("><", "+-", ",.", "[]")),
      Validated.valid(
        List(
          Lexeme.IncrementDataPointer,
          Lexeme.DecrementDataPointer,
          Lexeme.Increment,
          Lexeme.Decrement,
          Lexeme.Read,
          Lexeme.Write,
          Lexeme.JumpForwardEqualZero,
          Lexeme.JumpBackwardNotEqualZero
        ).zipWithIndex.map { case (lexeme, column) =>
          Token(lexeme, Location.Local(column / 2, column % 2))
        }
      )
    )
  }
  test("Lexer ignores whitespace") {
    assertEquals(
      lexer.lex(List(" +   +  +")),
      Validated.valid(
        List(
          Token(Lexeme.Increment, Location.Local(0, 1)),
          Token(Lexeme.Increment, Location.Local(0, 5)),
          Token(Lexeme.Increment, Location.Local(0, 8))
        )
      )
    )
  }

  test("Lexer reports invalid tokens") {
    assertEquals(
      lexer.lex(List("+I++J", "++K")),
      Validated.invalid(
        NonEmptyList.of(
          LexerError.InvalidToken('I', Location.Local(0, 1)),
          LexerError.InvalidToken('J', Location.Local(0, 4)),
          LexerError.InvalidToken('K', Location.Local(1, 2))
        )
      )
    )
  }

  test("Lexer ignores comments") {
    assertEquals(
      lexer.lex(List("+ // +", "- // Also ignored")),
      Validated.valid(
        List(
          Token(Lexeme.Increment, Location.Local(0, 0)),
          Token(Lexeme.Decrement, Location.Local(1, 0))
        )
      )
    )

  }
