package bfcompiler.util

object Logging {
  def commandExecution(value: String): Unit =
    println(s"[INFO] Executing command '$value'")
}
