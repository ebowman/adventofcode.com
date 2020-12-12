package y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

import scala.collection.immutable

class Day12Spec extends AnyFlatSpec with Matchers with Day12 {

  lazy val ins: immutable.Seq[String] =
    """
      |
      |F10
      |N3
      |F7
      |R90
      |F11
      |
      |""".stripMargin.trim.linesIterator.toIndexedSeq

  it should "solve part 1 test" in {
    Ship1().evolve(ins).manhattanDistance shouldBe 25
  }

  it should "solve part 1" in {
    Ship1().evolve(Loader(this, "day12.txt")).manhattanDistance shouldBe 1687
  }

  it should "solve part 2 test" in {
    Ship2().evolve(ins).manhattanDistance shouldBe 286
  }

  it should "solve part 2" in {
    Ship2().evolve(Loader(this, "day12.txt")).manhattanDistance shouldBe 20873
  }
}
