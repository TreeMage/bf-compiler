package bfcompiler.cli

import com.monovore.decline.{Command, Opts}

import scala.language.postfixOps

object CliApp {
  private val command = Command(
    name = "bf-compiler",
    header = "A brainfuck interpreter and compiler written in Scala 3."
  )(
    InterpretCommand.command
  )
  def run(args: Seq[String], env: Map[String, String]): Unit =
    command.parse(args, env) match
      case Left(help) if help.errors.isEmpty =>
        println(help)
        sys.exit(0)

      case Left(help) =>
        System.err.println(help)
        sys.exit(1)

      case Right(_) => ()
}
