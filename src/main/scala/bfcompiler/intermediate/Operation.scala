package bfcompiler.intermediate

import bfcompiler.common.Location

enum OperationType:
  case IncrementDataPointer
  case DecrementDataPointer
  case Increment
  case Decrement
  case Write
  case Read
  case JumpForwardEqualZero(targetAddress: Int)
  case JumpBackwardNotEqualZero(targetAddress: Int)
  case Noop

case class Operation(op: OperationType, location: Location)
