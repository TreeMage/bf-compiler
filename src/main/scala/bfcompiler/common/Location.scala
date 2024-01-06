package bfcompiler.common

import java.nio.file.Path

enum Location:
  case Local(line: Int, column: Int)
  case File(line: Int, column: Int, path: Path)

object Location:
  extension (ll: Location.Local)
    def inFile(path: Path): Location.File =
      Location.File(ll.line, ll.column, path)

  end extension
  extension (location: Location)
    def asString: String = location match
      case Location.Local(line, column)      => s"${line}:${column}"
      case Location.File(line, column, path) => s"${path}:${line}:${column}"
  end extension
