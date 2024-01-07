package bfcompiler.native

import bfcompiler.common.Program
import bfcompiler.intermediate.OperationType

trait NativeCompiler:
  def compile(program: Program): String

object NativeCompiler:
  val arm: NativeCompiler = new NativeCompiler:
    private val header =
      """.global _start
         |.align 2
         |
         |_start:
         |    // Push data pointer to the stack
         |    adrp x0, mem@PAGE
         |    add x0, x0, mem@PAGEOFF
         |    str x0, [sp, -16]!""".stripMargin
    private val data =
      s""".data
        |printf_fmt:
        |   .ascii "%hu\\n"
        |scanf_fmt:
        |   .ascii "%hu"
        |mem:
        |   .zero $MEMORY_CAPACITY""".stripMargin
    private val exitSyscall =
      s"""exit:
         |    mov x0, #0
         |    mov x16, #1
         |    svc 0""".stripMargin
    private def push(operand: String)       = s"str $operand, [sp, -16]!"
    private def pop(operand: String)        = s"ldr $operand, [sp], 16"
    private def getDPFromStack(reg: String) = s"ldr $reg, [sp]"
    override def compile(program: Program): String =
      val buffer = collection.mutable.ListBuffer.empty[String]
      val finalLabel =
        s"""addr_${program.ops.length}:
           |    nop""".stripMargin
      val assembly = program.ops.zipWithIndex.map { case (op, ip) =>
        val address_label = s"addr_$ip:"
        val instructions = op.op match
          case OperationType.IncrementDataPointer =>
            s"""// Increment data pointer
                 |${pop("x0")}
                 |add x0, x0, #1
                 |${push("x0")}""".stripMargin
          case OperationType.DecrementDataPointer =>
            s"""// Decrement data pointer
               |${pop("x0")}
               |sub x0, x0, #1
               |${push("x0")}""".stripMargin
          case OperationType.Increment =>
            s"""// Increment
               |${getDPFromStack("x0")}
               |ldr x1, [x0]
               |add x1, x1, #1
               |str x1, [x0]""".stripMargin
          case OperationType.Decrement =>
            s"""// Decrement
               |${getDPFromStack("x0")}
               |ldr x1, [x0]
               |sub x1, x1, #1
               |str x1, [x0]""".stripMargin
          case OperationType.Write =>
            s"""// Write
               |${getDPFromStack("x1")}
               |sub sp, sp, 0x10                ; Reserve 16 bytes on the stack
               |ldr x2, [x1]                    ; Load value at current data pointer
               |str x2, [sp]                    ; Store on stack
               |adrp x0, printf_fmt@PAGE        ; Load format string
               |add x0, x0, printf_fmt@PAGEOFF
               |bl _printf                      ; Call printf
               |
               |add sp, sp, 0x10                ; Release stack space
               |""".stripMargin
          case OperationType.Read =>
            s"""${getDPFromStack("x19")}
               |adrp x0, scanf_fmt@PAGE       ; Load format string
               |add x0, x0, scanf_fmt@PAGEOFF
               |mov x1, x19
               |bl _scanf                     ; Call scanf (pointer to buffer in x1)
               |ldr x0, [x19]
               |and x0, x0, #0xFF             ; Clamp value to [0, 255]
               |str x0, [x19]
               |""".stripMargin
          case OperationType.JumpForwardEqualZero(targetAddress) =>
            s"""// Jump Forward
               |${getDPFromStack("x0")}
               |ldr x1, [x0]
               |cbz x1, addr_${targetAddress + 1}
               """.stripMargin
          case OperationType.JumpBackwardNotEqualZero(targetAddress) =>
            s"""// Jump Backward
               |${getDPFromStack("x0")}
               |ldr x1, [x0]
               |cbnz x1, addr_${targetAddress + 1}
               """.stripMargin
          case OperationType.Noop => "nop"

        s"$address_label\n" + instructions.indent(4)
      }
      buffer
        .append(header)
        .appendAll(assembly)
        .append(finalLabel)
        .append(exitSyscall)
        .append(data)
        .mkString("\n")
  private val MEMORY_CAPACITY = 64_000
