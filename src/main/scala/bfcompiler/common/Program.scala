package bfcompiler.common

import bfcompiler.intermediate.Operation

case class Program(ops: List[Operation])

object Program:
  extension (program: Program)
    def asTree: String =
      program.ops.zipWithIndex
        .map { (op, index) =>
          s"$index. ${op.op} (${op.location.toStringLocation})"
        }
        .mkString("\n")
