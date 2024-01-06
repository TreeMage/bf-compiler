package bfcompiler.native

import bfcompiler.util.Logging

import java.nio.file.Path
import scala.sys.process.*
import scala.util.Try

enum LinkerError:
  case LinkingFailed(cause: Throwable)

trait ExternalLinkerWrapper:
  def compile(
      objectFilePath: Path,
      executableFilePath: Path,
      entryPoint: String
  ): Either[LinkerError, Int]

object ExternalLinkerWrapper:
  def ld(config: LdConfig): ExternalLinkerWrapper = new ExternalLinkerWrapper:
    def makeCommand(
        objectFilePath: Path,
        executableFilePath: Path,
        entryPoint: String
    ): String =
      s"ld -o $executableFilePath $objectFilePath -e $entryPoint ${config.toArgumentString}"
    override def compile(
        objectFilePath: Path,
        executableFilePath: Path,
        entryPoint: String
    ): Either[LinkerError, Int] =
      val command = makeCommand(objectFilePath, executableFilePath, entryPoint)
      Logging.commandExecution(command)
      Try(Process(command).!).toEither.left.map(LinkerError.LinkingFailed(_))
