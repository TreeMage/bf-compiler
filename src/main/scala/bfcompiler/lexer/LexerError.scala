package bfcompiler.lexer

import bfcompiler.common.Location

import java.nio.file.Path

enum LexerError:
  case IOError(path: Path, cause: Throwable)
  case InvalidToken(value: Char, location: Location)


