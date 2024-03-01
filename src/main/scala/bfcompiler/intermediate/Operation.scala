package bfcompiler.intermediate

import bfcompiler.common.Location

enum OperationType:
  case IncrementDataPointer(count: Int)
  case DecrementDataPointer(count: Int)
  case Increment(count: Int)
  case Decrement(count: Int)
  case Write
  case Read
  case JumpForwardEqualZero(targetAddress: Int)
  case JumpBackwardNotEqualZero(targetAddress: Int)

case class Operation(op: OperationType, location: Location)

object Operation:
  extension (op: Operation)
    def isSame(that: Operation): Boolean = op.op.ordinal == that.op.ordinal
