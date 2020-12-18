package y2020

import scala.util.parsing.combinator.JavaTokenParsers

trait Day18 extends JavaTokenParsers {

  trait Part1Parser extends JavaTokenParsers {
    def number: Parser[Long] = wholeNumber ^^ (_.toLong)

    def factor: Parser[Long] = number | "(" ~> expr <~ ")"

    def plusTerm: Parser[Long] = factor ~ "+" ~ factor ^^ { case x ~ _ ~ y => x + y }

    def timesTerm: Parser[Long] = factor ~ "*" ~ factor ^^ { case x ~ _ ~ y => x * y }

    def term: Parser[Long] = factor | plusTerm | timesTerm

    def expr: Parser[Long] = term ~ rep(("+" ~ term) | ("*" ~ term)) ^^ {
      case num ~ list => list.foldLeft(num) {
        case (x, "+" ~ y) => x + y
        case (x, "*" ~ y) => x * y
      }
    }
  }


  def part1(input: IndexedSeq[String]): Long = {
    val p = new Part1Parser {}
    input.map(line => p.parseAll(p.expr, line).get.toLong).sum
  }

  trait Part2Parser extends JavaTokenParsers {
    def number: Parser[Long] = wholeNumber ^^ (_.toLong)

    def factor: Parser[Long] = number | "(" ~> expr <~ ")"

    def plusTerm: Parser[Long] = factor ~ rep("+" ~> factor) ^^ { case x ~ y => y.foldLeft(x)(_ + _) }

    def term: Parser[Long] = plusTerm | factor

    def expr: Parser[Long] = term ~ rep(("*" ~> term)) ^^ { case x ~ y => y.foldLeft(x)(_ * _) }
  }
  def part2(input: IndexedSeq[String]): Long = {
    val p = new Part2Parser {}
    input.map(line => p.parseAll(p.expr, line).get.toLong).sum
  }
}


