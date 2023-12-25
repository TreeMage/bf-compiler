package bfcompiler.lexer

import bfcompiler.common
import bfcompiler.common.{Location, Token}

import java.nio.file.Path
import scala.util.Try

type LexerResult = Either[List[LexerError], List[Token]]
object LexerResult:
  def fromError(error: LexerError): LexerResult = fromErrors(List(error))
  def fromErrors(errors: List[LexerError]): LexerResult = Left(errors)

  def sequence(results: List[LexerResult]): LexerResult =
    results.partitionMap(identity) match
      case (Nil, tokens) => Right(tokens.flatten)
      case (errors, _)   => Left(errors.flatten)

trait Lexer:
  def lexFile(path: Path): LexerResult

object Lexer {
  val default: Lexer = new Lexer:
    import bfcompiler.util.Extensions.*
    def lexLine(line: String, lineIndex: Int, filePath: Path): LexerResult =
      val parsed = line.toList.zipWithIndex.map { case (value, column) =>
        val location = Location(filePath, lineIndex, column)
        value match
          case '>' => Right(common.Token(Lexeme.IncrementDataPointer, location))
          case '<' => Right(common.Token(Lexeme.DecrementDataPointer, location))
          case '+' => Right(common.Token(Lexeme.Increment, location))
          case '-' => Right(common.Token(Lexeme.Decrement, location))
          case '.' => Right(common.Token(Lexeme.Write, location))
          case ',' => Right(common.Token(Lexeme.Read, location))
          case '[' => Right(common.Token(Lexeme.JumpForwardEqualZero, location))
          case ']' =>
            Right(common.Token(Lexeme.JumpBackwardEqualZero, location))
          case ' '        => Right(common.Token(Lexeme.Empty, location))
          case _ @invalid => Left(LexerError.InvalidToken(invalid, location))
      }
      parsed.partitionMap(identity) match {
        case (Nil, rights) => Right(rights)
        case (lefts, _)    => Left(lefts)
      }

    override def lexFile(path: Path): LexerResult =
      val lexed =
        for source <- Try(scala.io.Source.fromFile(path.toFile)).toEither
        yield source.getLines().toList.zipWithIndex.map { case (line, index) =>
          lexLine(line, index, path)
        }
      lexed.fold(
        throwable => LexerResult.fromError(LexerError.IOError(path, throwable)),
        results =>
          results.sequence match
            case Left(value)  => Left(value.flatten)
            case Right(value) => Right(value.flatten)
      )
}
