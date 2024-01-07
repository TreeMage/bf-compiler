package bfcompiler.util

import java.io.FileInputStream
import java.nio.file.Path
import scala.util.Using

object FileIO:
  def readAllLines(path: Path): Either[IOError, List[String]] =
    Using(scala.io.BufferedSource(new FileInputStream(path.toFile)))(
      _.getLines().toList
    ).toEither.left.map(cause => IOError(cause, path))

  case class IOError(cause: Throwable, path: Path)
