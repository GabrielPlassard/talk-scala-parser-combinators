package net.compiletoi

import scala.util.parsing.combinator.RegexParsers

sealed trait Expression
case class Literal(v: Integer) extends Expression
case class Sum(left: Expression, right: Expression) extends Expression
case class Product(left: Expression, right: Expression) extends Expression

object ExpressionParser extends RegexParsers {
  def lit: Parser[Literal] = "\\d+".r ^^(str => Literal(str.toInt))
  def sum: Parser[Sum] = "(" ~> expr ~ "+" ~ expr <~ ")" ^^ { case a ~ _ ~ b => Sum(a,b)}
  def product: Parser[Product] = "(" ~> expr ~ "*" ~ expr <~ ")" ^^ { case a ~ _ ~ b => Product(a,b)}
  def expr: Parser[Expression] = lit | sum | product


  def apply(input: String): ParseResult[Expression] = parseAll(expr, input)
}
