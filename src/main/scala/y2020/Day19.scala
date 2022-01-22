package y2020

import scala.util.parsing.combinator.RegexParsers

trait Day19 extends RegexParsers {

  override def skipWhitespace = false

  trait Rule {
    val id: Int

    def build(dict: Map[Int, Rule]): String
  }

  var enablePart2 = false

  case class CoreRule(id: Int, pattern: String) extends Rule {
    def build(dict: Map[Int, Rule]): String = pattern
  }

  case class SeqRule(id: Int, patterns: Seq[Int]) extends Rule {
    def build(dict: Map[Int, Rule]): String = {
      (id, enablePart2) match {
        case (8, true) =>
          val p42 = dict(42).build(dict)
          s"($p42)+"
        case (11, true) =>
          // There's no general way to do this, but as the instructions say, "you only need to handle the
          // rules you have". In this case, "4" is enough.
          val p42 = dict(42).build(dict)
          val p31 = dict(31).build(dict)
          "(" + (for i <- 1 to 4 yield { s"($p42{$i}$p31{$i})" }).mkString("|") + ")"
        case _ =>
          patterns.map(id => dict(id).build(dict)).mkString
      }
    }
  }

  case class OrRule(id: Int, patterns1: Seq[Int], patterns2: Seq[Int]) extends Rule {
    def build(dict: Map[Int, Rule]): String = {
      s"((${patterns1.map(id => dict(id).build(dict)).mkString})|(${patterns2.map(id => dict(id).build(dict)).mkString}))"
    }
  }

  def isDigit(c: Char): Boolean = c >= '0' && c <= '9'

  def id: Parser[Int] =
    """\d+: """.r ^^ { _.takeWhile(isDigit).toInt }

  def seq: Parser[Seq[Int]] = rep1sep("""\d+""".r, " ") ^^ { _.map(_.toInt) }

  def orParser: Parser[(Seq[Int], Seq[Int])] = (seq <~ " | ") ~ seq ^^ { case s1 ~ s2 => (s1, s2) }

  def coreRule: Parser[CoreRule] = id ~ """"[a-z]"""".r ^^ { case id ~ value => CoreRule(id, value.tail.init) }

  def seqRule: Parser[SeqRule] = id ~ seq ^^ { case id ~ seq => SeqRule(id, seq) }

  def orRule: Parser[OrRule] = id ~ orParser ^^ { case id ~ or => OrRule(id, or._1, or._2) }

  def ruleParser: Parser[Rule] = orRule | seqRule | coreRule

  def parseDict(input: IndexedSeq[String]): Map[Int, Rule] = {
    input.takeWhile(_.contains(":")).map { line =>
      val rule = parseAll(ruleParser, line).get
      rule.id -> rule
    }.toMap
  }

  def part1(input: IndexedSeq[String]): Int = {
    val dict = parseDict(input)
    val r = dict(0).build(dict).r
    input.dropWhile(_.contains(":")).tail.count(line => r.pattern.matcher(line).matches())
  }
}
