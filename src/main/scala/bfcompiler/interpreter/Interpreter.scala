package bfcompiler.interpreter

import bfcompiler.common.{Program, Token}
import bfcompiler.intermediate.{Operation, OperationType}
import bfcompiler.lexer.Lexeme.*
import cats.implicits.*

trait Interpreter:
  def run(program: Program): Either[InterpreterError, Unit]

object Interpreter:
  val default: Interpreter = new Interpreter:
    val MEMORY_SIZE = 30_000
    override def run(program: Program): Either[InterpreterError, Unit] =
      var ip   = 0
      var dp   = 0
      val data = Array.fill[Byte](MEMORY_SIZE)(0)
      while (ip < program.ops.length)
        val operation = program.ops(ip)
        operation.op match
          case OperationType.IncrementDataPointer =>
            dp = dp + 1
            if (dp >= data.length)
              return InterpreterError
                .DataPointerOutOfBounds(dp, operation)
                .asLeft
            ip = ip + 1
          case OperationType.DecrementDataPointer =>
            dp = dp - 1
            if (dp < 0)
              return InterpreterError
                .DataPointerOutOfBounds(dp, operation)
                .asLeft
            ip = ip + 1
          case OperationType.Increment =>
            data(dp) = ((data(dp) + 1) % 255).toByte
            ip = ip + 1
          case OperationType.Decrement =>
            data(dp) = ((data(dp) - 1) % 255).toByte
            ip = ip + 1
          case OperationType.Write =>
            println(data(dp))
            ip = ip + 1
          case OperationType.Read =>
            data(dp) = scala.io.StdIn.readByte()
            ip = ip + 1
          case OperationType.JumpForwardEqualZero(targetAddress) =>
            if (data(dp) == 0) ip = targetAddress + 1
            else ip = ip + 1
          case OperationType.JumpBackwardEqualZero(targetAddress) =>
            if (data(dp) != 0) ip = targetAddress + 1
            else ip = ip + 1
          case OperationType.Noop =>
            ip = ip + 1

      ().asRight
