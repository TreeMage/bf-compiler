package bfcompiler.intermediate
import bfcompiler.common.{Location, Program, Token}
import bfcompiler.lexer.Lexeme.*
import cats.data.{NonEmptyList, Validated, ValidatedNel}

type IntermediateCompilationResult =
  ValidatedNel[IntermediateCompilationError, Program]
trait IntermediateCompiler:
  def compile(tokens: List[Token]): IntermediateCompilationResult

object IntermediateCompiler:
  val default: IntermediateCompiler = new IntermediateCompiler:
    type Accumulator =
      ValidatedNel[IntermediateCompilationError, List[Operation]]
    type State = (List[(Int, Int)], List[Int], Accumulator)
    extension (acc: Accumulator)
      def addSimpleToken(
          opType: OperationType,
          location: Location
      ): Accumulator =
        acc.map(Operation(opType, location) +: _)

    def matchLoopTokens(
        tokens: List[Token]
    ): ValidatedNel[IntermediateCompilationError, List[(Int, Int)]] =
      type MatchLoopAccumulator = (
          List[(Int, Location)],
          List[(Int, Int)],
          List[IntermediateCompilationError]
      )
      val resolveResult = tokens.zipWithIndex.foldLeft[MatchLoopAccumulator](
        (List.empty, List.empty, List.empty)
      ) { case ((stack, acc, errors), (token, index)) =>
        token.lexeme match
          case JumpForwardEqualZero =>
            ((index, token.location) +: stack, acc, errors)
          case JumpBackwardNotEqualZero =>
            stack.headOption match
              case Some((startIndex, _)) =>
                (stack.tail, (startIndex, index) +: acc, errors)
              case None =>
                (
                  stack,
                  acc,
                  IntermediateCompilationError.UnmatchedEndLoop(
                    token.location
                  ) +: errors
                )
          case _ => (stack, acc, errors)
      }
      val (stack, acc, errors) = resolveResult
      val combined_errors = errors.reverse ++ stack.map { case (_, location) =>
        IntermediateCompilationError.UnmatchedStartLoop(location)
      }
      NonEmptyList.fromList(combined_errors) match
        case Some(errors) => Validated.invalid(errors)
        case None         => Validated.validNel(acc)

    override def compile(tokens: List[Token]): IntermediateCompilationResult =
      matchLoopTokens(tokens) match
        case failure @ Validated.Invalid(_) => failure
        case Validated.Valid(loopPairings) =>
          tokens.zipWithIndex
            .foldLeft[State](
              (loopPairings, List.empty, Validated.valid(List.empty))
            ) { case ((pairingStack, startIndexStack, acc), (token, index)) =>
              token.lexeme match
                case IncrementDataPointer =>
                  (
                    pairingStack,
                    startIndexStack,
                    acc.addSimpleToken(
                      OperationType.IncrementDataPointer(1),
                      token.location
                    )
                  )
                case DecrementDataPointer =>
                  (
                    pairingStack,
                    startIndexStack,
                    acc.addSimpleToken(
                      OperationType.DecrementDataPointer(1),
                      token.location
                    )
                  )
                case Increment =>
                  (
                    pairingStack,
                    startIndexStack,
                    acc.addSimpleToken(
                      OperationType.Increment(1),
                      token.location
                    )
                  )
                case Decrement =>
                  (
                    pairingStack,
                    startIndexStack,
                    acc.addSimpleToken(
                      OperationType.Decrement(1),
                      token.location
                    )
                  )
                case Read =>
                  (
                    pairingStack,
                    startIndexStack,
                    acc.addSimpleToken(OperationType.Read, token.location)
                  )
                case Write =>
                  (
                    pairingStack,
                    startIndexStack,
                    acc.addSimpleToken(OperationType.Write, token.location)
                  )
                case JumpForwardEqualZero =>
                  val (start, end) = pairingStack.head
                  (
                    pairingStack.tail,
                    start +: startIndexStack,
                    acc.addSimpleToken(
                      OperationType.JumpForwardEqualZero(end),
                      token.location
                    )
                  )
                case JumpBackwardNotEqualZero =>
                  val start = startIndexStack.head
                  (
                    pairingStack,
                    startIndexStack.tail,
                    acc.addSimpleToken(
                      OperationType.JumpBackwardNotEqualZero(start),
                      token.location
                    )
                  )
            }
            ._3
            .map(ops => Program(ops.reverse))
