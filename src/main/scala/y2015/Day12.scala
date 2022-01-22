package y2015

import scala.util.parsing.combinator.JavaTokenParsers

trait Day12 extends JavaTokenParsers {

  private val Num = """-?\d+""".r

  def countNums(str: String): Int = Num.findAllIn(str).map(_.toInt).sum

  def trimQuotes(s: String): String = s.tail.init

  def expr: Parser[Json] = (
    obj
      | arr
      | stringLiteral ^^ { x => JsonString(trimQuotes(x)) }
      | floatingPointNumber ^^ (s => JsonNumber(BigDecimal(s)))
    )

  def obj: Parser[JsonObject] = "{" ~> repsep(member, ",") <~ "}" ^^ this.JsonObject.apply

  def arr: Parser[JsonArray] = "[" ~> repsep(expr, ",") <~ "]" ^^ this.JsonArray.apply

  def member: Parser[(String, Json)] = (stringLiteral <~ ":") ~ expr ^^ {
    case k ~ v => trimQuotes(k) -> v
  }

  def count(json: String): Int = parseAll(expr, json).get.count

  trait Json {
    def count: Int = 0
  }

  case class JsonBoolean(b: Boolean) extends Json

  case class JsonString(s: String) extends Json

  case class JsonNumber(x: BigDecimal) extends Json {
    override def count: Int = x.toInt
  }

  case class JsonArray(elems: List[Json]) extends Json {
    override def count: Int = elems.map(_.count).sum
  }

  case class JsonObject(entries: List[(String, Json)]) extends Json {
    override def count: Int = {
      if entries.exists(_._2 == JsonString("red")) then {
        0
      } else {
        (entries.collect {
          case (_, obj) => obj.count
        }).sum
      }
    }
  }

  case object JsonNull extends Json
}
