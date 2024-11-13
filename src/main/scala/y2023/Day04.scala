package y2023

import scala.annotation.tailrec
import scala.util.parsing.combinator.JavaTokenParsers

trait CardParser extends JavaTokenParsers {
  def card: Parser[(Set[Int], Set[Int])] = """Card\s+\d+:""".r ~> run2

  def run2: Parser[(Set[Int], Set[Int])] = (run <~ "|") ~ run ^^ { case a ~ b => (a, b) }

  def run: Parser[Set[Int]] = rep(wholeNumber) ^^ {
    _.map(_.toInt).toSet
  }
}

// see https://adventofcode.com/2023/day/4
trait Day04 extends CardParser {
  def solvePart1(input: Seq[String]): Int = {
    input.map(line => parseAll(card, line).get).map { case (a, b) =>
      val matches = a.intersect(b).size
      if (matches == 0) 0 else 1 << (matches - 1)
    }.sum
  }

  def solvePart2(input: Seq[String]): Int = {
    val matchMap = input.map(line => parseAll(card, line).get).toIndexedSeq.map { case (a, b) =>
      a.intersect(b).size
    }
    
    val winStructure = (for (cardNo <- 1 to input.size) yield {
      val matches = matchMap(cardNo - 1)
      if matches == 0 then None
      else Some(cardNo -> (cardNo + 1 to cardNo + matches))
    }).flatten.toMap

    import collection.mutable
    @tailrec def recurse(cards: mutable.Queue[Int], count: Int = 0): Int = {
      if cards.isEmpty then count
      else {
        val next = cards.dequeue()
        if matchMap(next - 1) == 0 then recurse(cards, count + 1)
        else recurse(cards.enqueueAll(winStructure(next)), count + 1)
      }
    }

    recurse((1 to input.size).foldLeft(mutable.Queue[Int]())(_ += _))
  }
}
