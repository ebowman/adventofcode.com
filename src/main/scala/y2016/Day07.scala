package y2016

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

trait Day07 {

  object parser extends RegexParsers {
    override val whiteSpace: Regex = "".r

    def tokens: Parser[String] = """\w+""".r

    def tokenPair: Parser[(String, String)] = (tokens <~ "[") ~ (tokens <~ "]") ^^ {
      case has ~ hasNot => (has, hasNot)
    }

    def sequence: Parser[(Seq[String], Seq[String])] = rep1(tokenPair) ~ opt(tokens) ^^ {
      case x ~ y => (x.map(_._1) ++ y, x.map(_._2))
    }
    def apply(input: String): ParseResult[(Seq[String], Seq[String])] = parseAll(sequence, input)
  }

  def solve1(input: Seq[String]): Int = {
    def isABBA(str: String): Boolean = str(0) == str(3) && str(1) == str(2) && str(0) != str(1)

    def hasAbba(str: String): Boolean = str.sliding(4).count(isABBA) > 0

    input.count { line =>
      parser(line).map { case (has, hasNot) =>
        has.exists(hasAbba) && hasNot.count(hasAbba) == 0
      }.getOrElse(false)
    }
  }

  def solve2(input: Seq[String]): Int = {
    def isABA(str: String) = str(0) == str(2) && str(0) != str(1)

    def findABAs(str: String): Set[String] = str.sliding(3).filter(isABA).toSet

    def toBAB(aba: String): String = Seq(aba(1), aba(0), aba(1)).mkString

    input.count { line =>
      parser(line).map { case (has, hasNot) =>
        has.flatMap(findABAs).map(toBAB).exists(bab => hasNot.exists(_.contains(bab)))
      }.getOrElse(false)
    }
  }
}