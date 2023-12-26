package bfcompiler.native

import bfcompiler.common.Program

trait NativeCompiler:
  def compile(program: Program): String

object NativeCompiler:
  val arm: NativeCompiler = new NativeCompiler:
    extension (xs: collection.mutable.ListBuffer[String])
      def appendIndented(line: String): xs.type =
        xs.append(line.indent(4))
    private val header =
      """.global _start
         |.align 2
         |
         |_start:""".stripMargin
    private val exitSyscall =
      """|mov X0, #0
         |mov X16, #1
         |svc 0""".stripMargin
    override def compile(program: Program): String =
      val buffer = collection.mutable.ListBuffer.empty[String]
      buffer.append(header).appendIndented(exitSyscall).mkString("\n")
