package y2019

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers

trait Day14 extends RegexParsers {

  override def skipWhitespace: Boolean = false

  case class Ingredient(chemical: String, amount: Long)

  def ingredient: Parser[(String, Long)] = ("""\d+""".r <~ " ") ~ """[A-Z]+""".r ^^ { case n ~ c => (c, n.toLong) }

  def ingredients: Parser[Seq[(String, Long)]] = rep1sep(ingredient, ", ")

  def formula: Parser[((String, Long), Seq[(String, Long)])] =
    (ingredients <~ " => ") ~ ingredient ^^ { case a ~ b => b -> a }

  @tailrec private final def gcd(n: (Long, Long)): Long = if (n._2 == 0) n._1 else gcd((n._2, n._1 % n._2))

  // normal % in java/scala doesn't work, because we need a positive modulus always.
  def divmod(n: Long, d: Long): (Long, Long) = (if (n < 0) n / d - 1L else n / d, ((n % d) + d) % d)

  def parse(input: IndexedSeq[String]): Map[String, (Long, Map[String, Long])] = {
    (input.map(line => parseAll(formula, line).get) map { case (ing, ings) =>
      ing._1 -> (ing._2, ings.toMap)
    }).toMap
  }

  def solve(map: Map[String, (Long, Map[String, Long])], fuel: Long = 1): Long = {
    @tailrec def recurse(input: (Map[String, Long], Map[String, Long])): Long = {
      val (needs, have) = (input._1, input._2)
      if (needs.size == 1 && needs.head._1 == "ORE") needs.head._2
      else {
        val need = needs.filterNot(_._1 == "ORE").head._1
        val (amt: Long, ingredients) = map(need)
        val (used: Long, leftover: Long) = divmod(needs(need), amt) match {
          case (d, 0L) => (d, 0L) // completely used
          case (d, m) => (d + 1, m) // we need one more but we have leftovers
        }
        recurse(ingredients.foldLeft((needs - need, if (leftover == 0) have else have + (need -> (amt - leftover)))) {
          case ((need, have), (ing, amount)) =>
            (need + (ing -> (need.getOrElse(ing, 0L) + used * amount - have.getOrElse(ing, 0L))), have - ing)
        })
      }
    }

    recurse(Map("FUEL" -> fuel), Map.empty)
  }

  def part1(input: IndexedSeq[String]): Long = solve(parse(input))

  def part2(input: IndexedSeq[String]): Long = {
    val map = parse(input)
    val goal = 1000000000000L

    @tailrec def bound(low: Long, high: Long): (Long, Long) = {
      if (solve(map, low) > goal) bound(low / 10, high)
      else if (solve(map, high) < goal) bound(high, high * 10)
      else (low, high)
    }

    @tailrec def search(low: Long, high: Long): Long = {
      if (low >= high - 1) low
      else {
        val mid = (low + high) / 2
        val ore = solve(map, mid)
        if (ore < goal) search(mid, high)
        else if (ore > goal) search(low, mid)
        else low
      }
    }

    val (low, high) = bound(goal, 10 * goal)
    search(low, high)
  }
}
