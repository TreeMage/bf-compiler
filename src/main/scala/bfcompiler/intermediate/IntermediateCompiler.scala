package bfcompiler.intermediate

import bfcompiler.common.{Location, Program, Token}
import bfcompiler.lexer.Lexeme.{Decrement, DecrementDataPointer, Empty, Increment, IncrementDataPointer, JumpBackwardEqualZero, JumpForwardEqualZero, Read, Write}


type IntermediateCompilationResult = Either[List[IntermediateCompilationError], Program]
trait IntermediateCompiler:
  def compile(tokens: List[Token]): IntermediateCompilationResult

object IntermediateCompiler:
  val default: IntermediateCompiler = new IntermediateCompiler:
    import bfcompiler.util.Extensions._
    type X = List[Either[IntermediateCompilationError, Operation]]
    type Accumulator = (List[(Int,Int)], List[Int], X)
    extension (acc: X)
      def addSimpleToken(opType: OperationType, location: Location): X =
         Right(Operation(opType, location)) +: acc

    def matchLoopTokens(tokens: List[Token]): Either[List[IntermediateCompilationError],List[(Int, Int)]] =
      type MatchLoopAccumulator = (List[(Int, Location)], List[(Int, Int)], List[IntermediateCompilationError])
      val resolveResult = tokens.zipWithIndex.foldLeft[MatchLoopAccumulator]((List.empty, List.empty, List.empty)) {
        case ((stack, acc, errors), (token, index)) => token.lexeme match
          case JumpForwardEqualZero => ((index, token.location) +: stack, acc, errors)
          case JumpBackwardEqualZero =>
            stack.headOption match
              case Some((startIndex, _)) => (stack.tail, (startIndex, index) +: acc, errors)
              case None => (stack, acc, IntermediateCompilationError.UnmatchedEndLoop(token.location) +: errors)
          case _ => (stack, acc, errors)
      }
      val (stack, acc, errors) = resolveResult
      if (stack.nonEmpty || errors.nonEmpty)
        Left(errors.reverse ++ stack.map { case (_, location) => IntermediateCompilationError.UnmatchedStartLoop(location)})
      else
        Right(acc.reverse)

    override def compile(tokens: List[Token]): IntermediateCompilationResult =
      matchLoopTokens(tokens) match
        case Left(errors) => Left(errors)
        case Right(loopPairings) =>
          tokens.zipWithIndex.foldLeft[Accumulator]((loopPairings, List.empty, List.empty)) {
            case ((pairingStack, startIndexStack, acc), (token, index)) =>
              token.lexeme match
                case IncrementDataPointer => (pairingStack, startIndexStack, acc.addSimpleToken(OperationType.IncrementDataPointer, token.location))
                case DecrementDataPointer => (pairingStack, startIndexStack, acc.addSimpleToken(OperationType.DecrementDataPointer, token.location))
                case Increment => (pairingStack, startIndexStack, acc.addSimpleToken(OperationType.Increment, token.location))
                case Decrement => (pairingStack, startIndexStack, acc.addSimpleToken(OperationType.Decrement, token.location))
                case Read => (pairingStack, startIndexStack, acc.addSimpleToken(OperationType.Read, token.location))
                case Write => (pairingStack, startIndexStack, acc.addSimpleToken(OperationType.Write, token.location))
                case Empty => throw new IllegalStateException("Empty tokens should have been eliminated. This is a bug in the lexer.")
                case JumpForwardEqualZero =>
                  val (start, end) = loopPairings.head
                  (pairingStack.tail, start +: startIndexStack, acc.addSimpleToken(OperationType.JumpForwardEqualZero(end), token.location))
                case JumpBackwardEqualZero =>
                  val start = startIndexStack.head
                  (pairingStack, startIndexStack.tail, acc.addSimpleToken(OperationType.JumpBackwardEqualZero(start), token.location))
          }._3.sequence.map(ops => Program(ops.reverse))