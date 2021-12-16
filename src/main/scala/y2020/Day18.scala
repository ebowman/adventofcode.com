package y2020

import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers

trait Day18 extends JavaTokenParsers {

  val optWsRe: Regex = """\s*""".r

  trait Shared extends JavaTokenParsers {

    override def skipWhitespace = false

    def optWs[T](p: => Parser[T]): Parser[T] = optWsRe ~> p <~ optWsRe

    def number: Parser[Long] = wholeNumber ^^ (_.toLong)

    def factor: Parser[Long] = number | optWs("(") ~> expr <~ optWs(")")

    def expr: Parser[Long]
  }

  trait Part1Parser extends Shared {

    def expr: Parser[Long] = factor ~ rep((optWs("+") ~ factor) | (optWs("*") ~ factor)) ^^ {
      case num ~ list => list.foldLeft(num) {
        case (x, "+" ~ y) => x + y
        case (x, "*" ~ y) => x * y
        case err => sys.error(s"Unexpected $err")
      }
    }
  }

  def part1(input: IndexedSeq[String]): Long = {
    val p = new Part1Parser {}
    input.map(line => p.parseAll(p.expr, line).get).sum
  }

  trait Part2Parser extends Shared {

    def term: Parser[Long] = factor ~ rep(optWs("+") ~> factor) ^^ { case x ~ y => y.foldLeft(x)(_ + _) }

    def expr: Parser[Long] = term ~ rep((optWs("*") ~> term)) ^^ { case x ~ y => y.foldLeft(x)(_ * _) }
  }

  def part2(input: IndexedSeq[String]): Long = {
    val p = new Part2Parser {}
    input.map(line => p.parseAll(p.expr, line).get).sum
  }
}


