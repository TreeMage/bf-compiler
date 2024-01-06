package bfcompiler.util

import java.io.FileInputStream
import java.nio.file.Path
import scala.util.Using

object FileIO:
  def readAllLines(path: Path): Either[Throwable, List[String]] =
    Using(scala.io.BufferedSource(new FileInputStream(path.toFile)))(
      _.getLines().toList
    ).toEither
