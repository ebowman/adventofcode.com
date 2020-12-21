package y2020

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.parsing.combinator.RegexParsers

case class Ingredient(ingredients: List[String], allergans: List[String])

trait Parser extends RegexParsers {
  override def skipWhitespace: Boolean = false

  def ingredient: Parser[String] = """[a-z]+""".r

  def ingredients: Parser[List[String]] = rep1sep(ingredient, " ")

  def allergans: Parser[List[String]] = "(contains " ~> rep1sep(ingredient, ", ") <~ ")"

  def description: Parser[Ingredient] = (ingredients <~ " ") ~ allergans ^^ { case ing ~ all => Ingredient(ing, all) }

  def parse(input: Seq[String]): Seq[Ingredient] = input.map(line => parseAll(description, line).get)
}

trait Day21 extends Parser {

  def findImpossibles(ingredients: Seq[Ingredient]): Map[String, String] = {
    val counter = new mutable.HashMap[String, Int]() {
      override def default(key: String): Int = 0
    }
    val maybes = new mutable.HashMap[String, mutable.Set[String]]()
    for (ingredient <- ingredients) {
      ingredient.ingredients.foreach(ingredient => counter(ingredient) += 1)
      ingredient.allergans.foreach { allergan =>
        if (maybes.contains(allergan)) maybes(allergan) = maybes(allergan).intersect(ingredient.ingredients.toSet)
        else maybes(allergan) = new mutable.HashSet[String]() ++ ingredient.ingredients
      }
    }

    @tailrec
    def recurse(maybes: mutable.Map[String, mutable.Set[String]],
                impossible: Map[String, String] = Map.empty): Map[String, String] = {
      if (maybes.isEmpty) impossible
      else {
        val (allergan, ingredients) = maybes.minBy(_._2.size)
        val next = ingredients.head
        maybes.foreach(maybe => maybe._2.remove(next))
        recurse(maybes - allergan, impossible + (allergan -> next))
      }
    }

    recurse(maybes)
  }

  def part1(input: IndexedSeq[String]): Int = {
    val ingredients = parse(input)
    val impossibles = findImpossibles(ingredients).values.toSet
    ingredients.map(ingredient => ingredient.copy(ingredients =
      ingredient.ingredients.filterNot(impossibles.contains))).flatMap(_.ingredients).size
  }

  def part2(input: IndexedSeq[String]): String = {
    val ingredients = parse(input)
    val impossibles = findImpossibles(ingredients)
    impossibles.toSeq.sortBy(_._1).map(_._2).mkString(",")
  }
}
