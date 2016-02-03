package net.compiletoi

import scala.util.parsing.combinator.RegexParsers

sealed trait Expression
case class Literal(v: Integer) extends Expression
case class Sum(left: Expression, right: Expression) extends Expression
case class Product(left: Expression, right: Expression) extends Expression

object ExpressionParser extends RegexParsers {
  def lit = """\d+""".r ^^ {i => Literal(i.toInt)}
  def sum = "(" ~> (expr <~ "+") ~ expr <~ ")" ^^ { case l ~ r => Sum(l, r)}
  def product = "(" ~> (expr <~ "*") ~ expr <~ ")" ^^ { case l ~ r => Product(l, r)}
  def expr: Parser[Expression] = lit | sum | product


  def apply(input: String): ParseResult[Expression] = parseAll(expr, input)
}
