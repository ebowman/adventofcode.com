package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day07Spec extends AnyFlatSpec with Matchers with Day07 {

  "Parser" should "handle the test case" in {
    val instructions =
      """
        |123 -> x
        |456 -> y
        |x AND y -> d
        |x OR y -> e
        |x LSHIFT 2 -> f
        |y RSHIFT 2 -> g
        |NOT x -> h
        |NOT y -> i""".stripMargin.trim.linesIterator

    instructions.foreach { instruction =>
      parseAll(parser, instruction).get
    }
    wires.toMap.mapValues(_.voltage) shouldBe Map(
      "d" -> 72, "e" -> 507, "f" -> 492, "g" -> 114, "h" -> 65412, "i" -> 65079, "x" -> 123, "y" -> 456)
  }

  it should "handle a harder test case" in {
    wires.clear()
    val instructions =
      """
        |123 -> x
        |456 -> y
        |d -> e
        |456 AND x -> d
        |e -> f
        |NOT y -> i""".stripMargin.trim.linesIterator

    instructions.foreach { instruction =>
      parseAll(parser, instruction).get
    }
    wires.toMap.mapValues(_.voltage) shouldBe Map("e" -> 72, "x" -> 123, "y" -> 456, "f" -> 72, "i" -> 65079, "d" -> 72)
  }

  it should "pass the final test" in {
    wires.clear()
    Loader(this, "day07.txt").foreach { instruction =>
      parseAll(parser, instruction).get
    }
    wires("a").voltage shouldBe 956

  }

  it should "allow for overriding" in {
    wires.clear()
    val instructions =
      """
        |123 -> x
        |456 -> x""".stripMargin.trim.linesIterator

    instructions.foreach { instruction =>
      parseAll(parser, instruction).get
    }
    wires("x").voltage shouldBe 456
  }

  it should "pass the second part" in {
    wires.clear()
    Loader(this, "day07.txt").foreach { instruction =>
      parseAll(parser, instruction).get
    }
    parseAll(parser, "956 -> b")
    wires("a").voltage shouldBe 40149
  }

}
