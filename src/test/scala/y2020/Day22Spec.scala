package y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader


class Day22Spec extends AnyFlatSpec with Matchers with Day22 {

  lazy val testInput: IndexedSeq[String] = Loader.is(this, "day22.test.txt")
  lazy val input: IndexedSeq[String] = Loader.is(this, "day22.txt")

  it should "solve part 1 test" in {
    part1(testInput) shouldBe 306
  }

  it should "solve part 1" in {
    part1(input) shouldBe 32102
  }

  it should "solve part 2 test" in {
    part2(testInput) shouldBe 291
  }

  it should "not be pathological" in {
    part2(
      """|Player 1:
         |43
         |19
         |
         |Player 2:
         |2
         |29
         |14""".stripMargin.trim.linesIterator.toIndexedSeq) shouldBe 105
  }

  it should "solve part 2" in {
    part2(input) shouldBe 34173
  }
}
