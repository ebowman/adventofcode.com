package y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day07Spec extends AnyFlatSpec with Matchers with Day07 {

  lazy val testInput: Iterable[String] =
    """
      |light red bags contain 1 bright white bag, 2 muted yellow bags.
      |dark orange bags contain 3 bright white bags, 4 muted yellow bags.
      |bright white bags contain 1 shiny gold bag.
      |muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
      |shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
      |dark olive bags contain 3 faded blue bags, 4 dotted black bags.
      |vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
      |faded blue bags contain no other bags.
      |dotted black bags contain no other bags.""".stripMargin.trim.linesIterator.iterator.to(Iterable)

  it should "parse the input" in {
    // "pale chartreuse bags contain 3 faded orange bags."
    parseAll(bag, "pale chartreuse bags").get shouldBe Bag("pale chartreuse")
    parseAll(bagCount, "7 yellow green bags").get shouldBe("yellow green", 7)
    parseAll(bagCount, "1 yellow green bag").get shouldBe("yellow green", 1)
    parseAll(bagList, "3 dark lavender bags, 3 mirrored coral bags, 1 dotted chartreuse bag").get shouldBe Seq(
      ("dark lavender", 3), ("mirrored coral", 3), ("dotted chartreuse", 1))
    parseAll(bagDefFull, "posh black bags contain 3 dark lavender bags, 3 mirrored coral bags, 1 dotted chartreuse bag.").get shouldBe Bag(
      "posh black", Map(("dark lavender", 3), ("mirrored coral", 3), ("dotted chartreuse", 1))
    )
    Loader(this, "day07.txt").foreach { line =>
      parseAll(bagDef, line).get
    }
  }

  it should "parse test input" in {
    val bags = testInput.map(line => parseAll(bagDef, line).get).toIndexedSeq
    containedBy("shiny gold", bags) shouldBe 4
  }

  it should "solve part 1" in {
    val input = Loader(this, "day07.txt")
    val bags = input.map(line => parseAll(bagDef, line).get).toIndexedSeq
    containedBy("shiny gold", bags) shouldBe 296
  }

  it should "solve part 2 test case" in {
    val bags: Map[String, Bag] = testInput.map(line => parseAll(bagDef, line).get).map(bag => (bag.name -> bag)).toMap
    solveContains("shiny gold", bags) shouldBe 32
  }

  it should "solve part 2" in {
    val input = Loader(this, "day07.txt")
    val bags = input.map(line => parseAll(bagDef, line).get).map(bag => (bag.name -> bag)).toMap
    solveContains("shiny gold", bags) shouldBe 9339
  }
}
