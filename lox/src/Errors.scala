package lox

case class SyntaxError(line: Int, where: String, message: String) extends Error(message)

case class ParseError(message: String) extends Error(message)
