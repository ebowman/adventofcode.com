package y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day17Spec extends AnyFlatSpec with Matchers with Day17 {
  it should "solve part 1 test" in {
    part1(
      """
        |.#.
        |..#
        |###""".stripMargin.trim.linesIterator.toIndexedSeq, 6) shouldBe 112
  }

  it should "solve part 1" in {
    part1(Loader(this, "day17.txt").toIndexedSeq, 6) shouldBe 401
  }

  it should "solve part 2 test" in {
    part2(
      """
        |.#.
        |..#
        |###""".stripMargin.trim.linesIterator.toIndexedSeq, 6) shouldBe 848
  }

  it should "solve part 2" in {
    part2(Loader(this, "day17.txt").toIndexedSeq, 6) shouldBe 2224
  }
}
