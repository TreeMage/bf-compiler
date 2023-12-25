package bfcompiler.common

import java.nio.file.Path

case class Location(filePath: Path, line: Int, column: Int)

object Location:
  extension (location: Location)
    def toStringLocation: String =
      s"${location.filePath}:${location.line}:${location.column}"
