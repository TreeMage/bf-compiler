package bfcompiler.common

import bfcompiler.common.Location
import bfcompiler.lexer.Lexeme

case class Token(lexeme: Lexeme, location: Location)


