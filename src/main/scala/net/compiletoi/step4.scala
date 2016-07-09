package net.compiletoi

import scala.util.parsing.combinator.RegexParsers

sealed trait JSON
case class JStr(v: String) extends JSON
case class JNum(v: Double) extends JSON
case class JBool(v: Boolean) extends JSON
case object JNull extends JSON
case class JArray(v: Seq[JSON]) extends JSON
case class JObject(v: Map[String, JSON]) extends JSON

object JSONParser extends RegexParsers {
  // for some inspiration, see http://www.json.org/

  def str: Parser[JStr] = "\"" ~> """[^"]*""".r <~"\"" ^^ JStr
  def num: Parser[JNum] = "\\d+\\.?\\d*".r ^^ {i => JNum(i.toDouble)}
  def bool: Parser[JBool] = ("true" | "false") ^^ {b => JBool(b.toBoolean)}
  def null_ : Parser[JSON] = "null" ^^^ JNull
  def array: Parser[JArray] = "[" ~> repsep(json, ",") <~ "]" ^^ JArray
  def item: Parser[(String, JSON)] = str ~ ":" ~ json ^^ { case k ~ _ ~ v => (k.v, v)}
  def object_ : Parser[JObject] = "{" ~> repsep(item, ",") <~ "}" ^^ {items => JObject(items.toMap)}


  def json: Parser[JSON] = str | num | bool | null_ | array | object_


  def apply(input: String): ParseResult[JSON] = parseAll(json, input)
}
