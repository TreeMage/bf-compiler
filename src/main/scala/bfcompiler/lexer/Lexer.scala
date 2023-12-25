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
    def lexLine(line: String, lineIndex: Int, filePath: Path): LexerResult =
      val parsed = line.toList.zipWithIndex.map { case (value, column) =>
        val location = Location(filePath, lineIndex, column)
        value match
          case '>' => Validated.valid(common.Token(Lexeme.IncrementDataPointer, location))
          case '<' => Validated.valid(common.Token(Lexeme.DecrementDataPointer, location))
          case '+' => Validated.valid(common.Token(Lexeme.Increment, location))
          case '-' => Validated.valid(common.Token(Lexeme.Decrement, location))
          case '.' => Validated.valid(common.Token(Lexeme.Write, location))
          case ',' => Validated.valid(common.Token(Lexeme.Read, location))
          case '[' => Validated.valid(common.Token(Lexeme.JumpForwardEqualZero, location))
          case ']' =>
            Validated.valid(common.Token(Lexeme.JumpBackwardEqualZero, location))
          case ' '        => Validated.valid(common.Token(Lexeme.Empty, location))
          case _ @invalid => Validated.invalidNel(LexerError.InvalidToken(invalid, location))
      }
      parsed.sequence

    override def lexFile(path: Path): LexerResult =
      val lexed =
        for source <- Try(scala.io.Source.fromFile(path.toFile)).toEither.toValidated
        yield source.getLines().toList.zipWithIndex.map { case (line, index) =>
          lexLine(line, index, path)
        }
      lexed match
        case Validated.Valid(tokens) => tokens.sequence.map(_.flatten)
        case Validated.Invalid(e) => Validated.invalidNel(LexerError.IOError(path, e))

}
