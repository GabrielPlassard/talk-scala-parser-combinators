package net.compiletoi

import scala.util.parsing.combinator.RegexParsers


object CSVParser extends RegexParsers {
  def regex = "[^\n;]*".r
  def cell: Parser[String] = regex
  def line = repsep(regex, ";")
  def csv = repsep(line, "\n")

  def apply(input: String): ParseResult[List[List[String]]] = parseAll(csv, input)
  override def skipWhitespace = false
}
