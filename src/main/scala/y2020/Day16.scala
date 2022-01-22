package y2020

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers

trait Day16 extends RegexParsers {

  override def skipWhitespace: Boolean = false

  case class Rule(name: String, r1: Range, r2: Range) {
    def valid(num: Int): Boolean = r1.contains(num) || r2.contains(num)
  }

  case class Ticket(fields: IndexedSeq[Int]) {
    def valid(rules: Seq[Rule]): Boolean = fields.forall(field => rules.exists(rule => rule.valid(field)))
  }

  def range: Parser[Range] = ("""\d+""".r <~ "-") ~ """\d+""".r ^^ { case min ~ max => min.toInt to max.toInt }

  def rule: Parser[Rule] = (("""[a-z ]+""".r <~ ": ") ~ (range <~ " or ")) ~ range ^^ { case name ~ r1 ~ r2 => Rule(name, r1, r2) }

  def ticket: Parser[Ticket] = repsep("""\d+""".r, ",") ^^ { nums => Ticket(nums.map(_.toInt).toIndexedSeq) }

  def parse(lines: Iterable[String]): (IndexedSeq[Rule], Ticket, IndexedSeq[Ticket]) = {
    (lines.map(line => parseAll(rule, line)).takeWhile(_.successful).map(_.get).toIndexedSeq,
      parseAll(ticket, lines.dropWhile(_ != "your ticket:").tail.head).get,
      lines.dropWhile(_ != "nearby tickets:").tail.map(line => parseAll(ticket, line).get).toIndexedSeq)
  }

  def part1(input: IndexedSeq[String]): Int = {
    val (rules, _, nearby) = parse(input)
    (for
      near <- nearby
      field <- near.fields if rules.forall(_.valid(field) == false)
    yield {
      field
    }).sum
  }

  def part2(input: IndexedSeq[String]): Long = {
    val (rules, mine, nearby) = parse(input)
    val validTickets = nearby.filter(_.valid(rules))

    // possible rules that could map the field indexed by the key
    val groups = (for
      i <- 0 until validTickets.map(_.fields.size).max
      rule <- rules if validTickets.forall(ticket => rule.valid(ticket.fields(i)))
    yield (i, rule)).groupBy(_._1).view.mapValues(_.map(_._2).toSet)

    // recurse until there is 1 row per field left. To do that we find the "next"
    // set of rules with one one rule in it, and then remove that rule from all
    // the others. We keep track of which rules we have already processed in 'done'.
    @tailrec
    def recurse(groups: Map[Int, Set[Rule]], done: Set[Rule] = Set.empty): Map[Int, Rule] = {
      if groups.forall(_._2.size == 1) then groups.view.mapValues(_.head).toMap
      else {
        val (newRules, filteredRule) =
          groups.find(p => p._2.size == 1 && !done.contains(p._2.head)).get match {
            case (i, rule) =>
              (
                groups.map {
                  case (j, _) if i == j => (i, rule)
                  case (j, rules) => (j, rules.filterNot(_ == rule.head))
                },
                rule
              )
          }
        recurse(newRules, done + filteredRule.head)
      }
    }

    // recurse until 1 rule per field
    val result = recurse(groups.toMap)

    // pull out all the fields from my ticket that start with "departure"
    val indices = for i <- result.keys if result(i).name.startsWith("departure") yield i

    // return the product
    indices.map(i => mine.fields(i)).map(_.toLong).product
  }
}