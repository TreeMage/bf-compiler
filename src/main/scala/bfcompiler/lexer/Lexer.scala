package bfcompiler.lexer

import bfcompiler.common
import bfcompiler.common.{Location, Token}

import java.nio.file.Path
import scala.util.Try
import cats.*
import cats.data.{Validated, ValidatedNel}
import cats.implicits.*

type LexerResult = ValidatedNel[LexerError, List[Token]]

trait Lexer:
  def lexFile(path: Path): LexerResult

object Lexer {
  val default: Lexer = new Lexer:
    extension (xs: List[ValidatedNel[LexerError, Token]])
      def prependToken(token: Token): List[ValidatedNel[LexerError, Token]] =
        Validated.valid(token) +: xs

    def lexLine(line: String, lineIndex: Int, filePath: Path): LexerResult =
      val parsed = line
        .split("//", 2)(0) // Ignore comments
        .toList
        .zipWithIndex
        .foldLeft(List.empty[ValidatedNel[LexerError, Token]]) {
          case (acc, (value, column)) =>
            val location = Location(filePath, lineIndex, column)
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
                acc prependToken Token(Lexeme.JumpBackwardEqualZero, location)
              case ' ' => acc
              case _ @invalid =>
                Validated.invalidNel(
                  LexerError.InvalidToken(invalid, location)
                ) +: acc
        }
      parsed.reverse.sequence

    override def lexFile(path: Path): LexerResult =
      val lexed =
        for source <- Try(
            scala.io.Source.fromFile(path.toFile)
          ).toEither.toValidated
        yield source.getLines().toList.zipWithIndex.map { case (line, index) =>
          lexLine(line, index, path)
        }
      lexed match
        case Validated.Valid(tokens) => tokens.sequence.map(_.flatten)
        case Validated.Invalid(e) =>
          Validated.invalidNel(LexerError.IOError(path, e))

}
