package y2020

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers

trait Day07 extends RegexParsers {

  def bagDef: Parser[Bag] = bagDefFull | bagDefEmpty

  def bagDefFull: Parser[Bag] = (bag <~ "contain") ~ bagList <~ "." ^^ { case bag ~ list => bag.copy(contains = list.toMap) }

  def bagList: Parser[Seq[(String, Int)]] = rep1sep(bagCount, ",")

  def bagCount: Parser[(String, Int)] = "\\d+".r ~ bag ^^ { case n ~ b => (b.name, n.toInt) }

  def bag: Parser[Bag] = ("[a-z]+".r ~ "[a-z]+".r) <~ ("bags" | "bag") ^^ { case c1 ~ c2 => Bag(s"$c1 $c2") }

  def bagDefEmpty: Parser[Bag] = bag <~ "contain no other bags." ^^ { b => b }

  def containedBy(name: String, bags: Seq[Bag]): Int = {
    def getParent(bag: Bag): Seq[Bag] = bags.filter(_.contains.keySet.contains(bag.name))

    @tailrec
    def recurse(bags: Seq[Bag], parents: Seq[Bag] = Seq.empty): Seq[Bag] = {
      if (bags.isEmpty) parents
      else {
        val newParents = bags.flatMap(getParent).distinct
        recurse(newParents, (parents ++ newParents).distinct)
      }
    }

    recurse(Seq(Bag(name))).size
  }

  def solveContains(name: String, bags: Map[String, Bag]): Int = {
    def recurse(innerBag: Bag): Int = {
      val children: Map[Bag, Int] = innerBag.contains.map { case (name, count) => bags(name) -> count }
      1 + children.map { case (childBag, count) => count * recurse(childBag) }.sum
    }

    recurse(bags(name)) - 1
  }

  case class Bag(name: String, contains: Map[String, Int] = Map.empty)
}
