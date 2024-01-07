package bfcompiler.lexer

enum Lexeme:
  case IncrementDataPointer
  case DecrementDataPointer
  case Increment
  case Decrement
  case Write
  case Read
  case JumpForwardEqualZero
  case JumpBackwardNotEqualZero
