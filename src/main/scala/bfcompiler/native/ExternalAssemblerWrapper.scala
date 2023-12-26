package bfcompiler.native

import bfcompiler.common.Program
import bfcompiler.util.Logging

import java.nio.file.Path
import scala.language.postfixOps
import scala.util.{Failure, Success, Try, Using}
import sys.process.*
import cats.implicits.*

import java.io.PrintWriter

enum AssemblerError:
  case IOError(cause: Throwable)
  case AssemblingFailed(cause: Throwable)

case class AssemblerResult(exitCode: Int, objectFilePath: Path)

trait ExternalAssemblerWrapper:
  def compile(
      assembly: String,
      outputDirectory: Path,
      outputFileNameWithoutExtension: String
  ): Either[AssemblerError, AssemblerResult]

object ExternalAssemblerWrapper:
  val arm64macos: ExternalAssemblerWrapper = new ExternalAssemblerWrapper:
    override def compile(
        assembly: String,
        outputDirectory: Path,
        outputFileNameWithoutExtension: String
    ): Either[AssemblerError, AssemblerResult] =
      val asmFilePath =
        outputDirectory.resolve(s"${outputFileNameWithoutExtension}.asm")
      val objectFilePath =
        outputDirectory.resolve(s"${outputFileNameWithoutExtension}.o")
      for
        _ <- Using(
          new PrintWriter(asmFilePath.toFile)
        )(_.write(assembly)).toEither.left.map(AssemblerError.IOError(_))
        command = s"as -o $objectFilePath $asmFilePath"
        _ <- Logging.commandExecution(command).asRight
        exitCode <- Try(Process(command).!).toEither.left
          .map(AssemblerError.AssemblingFailed(_))
      yield AssemblerResult(exitCode, objectFilePath)
