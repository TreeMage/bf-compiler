package bfcompiler.intermediate

import bfcompiler.common.Location

enum IntermediateCompilationError:
  case UnmatchedStartLoop(location: Location)
  case UnmatchedEndLoop(location: Location)

