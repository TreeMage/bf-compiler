package bfcompiler.optimizer

import bfcompiler.common.Program
import bfcompiler.intermediate.{Operation, OperationType}
import bfcompiler.intermediate.Operation.*
import bfcompiler.intermediate.OperationType.Increment
import cats.data.{NonEmptyList}

trait Optimizer:
  def optimize(program: Program): Program

object Optimizer:
  private type CollapsableOperation = OperationType.Increment |
    OperationType.Decrement | OperationType.IncrementDataPointer |
    OperationType.DecrementDataPointer;

  def default: Optimizer = new Optimizer:
    private def collapseOperationGroup(
        group: NonEmptyList[Operation]
    ): Operation =
      val count    = group.length
      val location = group.head.location
      group.head.op match
        case _: OperationType.Increment =>
          Operation(OperationType.Increment(count), location)
        case _: OperationType.Decrement =>
          Operation(OperationType.Decrement(count), location)
        case _: OperationType.IncrementDataPointer =>
          Operation(OperationType.IncrementDataPointer(count), location)
        case _: OperationType.DecrementDataPointer =>
          Operation(OperationType.DecrementDataPointer(count), location)
        case _ =>
          throw new IllegalStateException(
            s"Can only collapse operations of type Increment, Decrement, IncrementDataPointeR and DecerementDataPointer but received ${group.head.op}."
          )

    private def collapseOperations(ops: List[Operation]): List[Operation] =
      type Accumulator = (List[Operation], Option[NonEmptyList[Operation]])
      val (collapsedOps, finalGroup) =
        ops.foldLeft[Accumulator]((List.empty, None)) {
          case ((processedOps, currentGroup), op) =>
            currentGroup match
              case Some(group) =>
                if (group.head.isSame(op)) (processedOps, Some(group :+ op))
                else
                  val collasped = collapseOperationGroup(group)
                  (processedOps :+ collasped :+ op, None)
              case None =>
                op.op match
                  case _: CollapsableOperation =>
                    (processedOps, Some(NonEmptyList.of(op)))
                  case _ => (processedOps.appended(op), None)
        }
      finalGroup match
        case Some(group) => collapsedOps :+ collapseOperationGroup(group)
        case None        => collapsedOps

    override def optimize(program: Program): Program =
      Program(collapseOperations(program.ops))
