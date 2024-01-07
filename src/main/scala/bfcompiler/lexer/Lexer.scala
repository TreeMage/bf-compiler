package bfcompiler.lexer

import bfcompiler.common
import bfcompiler.common.{Location, Token}
import cats.*
import cats.data.{Validated, ValidatedNel}
import cats.implicits.*

type LexerResult = ValidatedNel[LexerError, List[Token]]

trait Lexer:
  def lex(lines: List[String]): LexerResult

object Lexer {
  val default: Lexer = new Lexer:
    extension (xs: List[ValidatedNel[LexerError, Token]])
      def prependToken(token: Token): List[ValidatedNel[LexerError, Token]] =
        Validated.valid(token) +: xs

    def lexLine(line: String, lineIndex: Int): LexerResult =
      val parsed = line
        .split("//", 2)(0) // Ignore comments
        .toList
        .zipWithIndex
        .foldLeft(List.empty[ValidatedNel[LexerError, Token]]) {
          case (acc, (value, column)) =>
            val location = Location.Local(lineIndex, column)
            value match
              case '>' =>
                acc prependToken Token(Lexeme.IncrementDataPointer, location)
              case '<' =>
                acc prependToken Token(Lexeme.DecrementDataPointer, location)
              case '+' => acc prependToken Token(Lexeme.Increment, location)
              case '-' => acc prependToken Token(Lexeme.Decrement, location)
              case '.' => acc prependToken Token(Lexeme.Write, location)
              case ',' => acc prependToken Token(Lexeme.Read, location)
              case '[' =>
                acc prependToken Token(Lexeme.JumpForwardEqualZero, location)
              case ']' =>
                acc prependToken Token(
                  Lexeme.JumpBackwardNotEqualZero,
                  location
                )
              case ' ' => acc
              case _ @invalid =>
                Validated.invalidNel(
                  LexerError.InvalidToken(invalid, location)
                ) +: acc
        }
      parsed.reverse.sequence

    override def lex(lines: List[String]): LexerResult =
      val lexed = lines.zipWithIndex.map { case (line, index) =>
        lexLine(line, index)
      }
      lexed.sequence.map(_.flatten)

}
