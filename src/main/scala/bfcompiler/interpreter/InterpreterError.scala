package bfcompiler.interpreter

import bfcompiler.intermediate.Operation

enum InterpreterError:
  case DataPointerOutOfBounds(index: Int, cause: Operation)
