package y2015

import scala.util.parsing.combinator.JavaTokenParsers

trait Day12 extends JavaTokenParsers {

  def solve1(str: String): Int =
    """-?\d+""".r.findAllIn(str).map(_.toInt).sum

  def solve2(json: String): Int = JsonParser.parseAll(JsonParser.expr, json).get.count

  object JsonParser extends JavaTokenParsers {

    import JsonObjectMode._

    def trimQuotes(s: String): String = s.tail.init

    def expr: Parser[Json] =
      obj | arr | stringLiteral ^^ (trimQuotes andThen JsonString.apply) |
        floatingPointNumber ^^ ((BigDecimal.apply : String => BigDecimal) andThen JsonNumber.apply)

    def obj: Parser[JsonObject] = "{" ~> repsep(member, ",") <~ "}" ^^ JsonObject.apply

    def arr: Parser[JsonArray] = "[" ~> repsep(expr, ",") <~ "]" ^^ JsonArray.apply

    def member: Parser[(String, Json)] = (stringLiteral <~ ":") ~ expr ^^ { case k ~ v => trimQuotes(k) -> v }
  }

  object JsonObjectMode {
    sealed trait Json {
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
      override def count: Int =
        if entries.exists(_._2 == JsonString("red")) then 0 else entries.map(_._2.count).sum
    }

    case object JsonNull extends Json
  }
}
