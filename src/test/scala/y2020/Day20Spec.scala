package y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader


class Day20Spec extends AnyFlatSpec with Matchers with Day20 {

  lazy val testInput: IndexedSeq[String] = Loader(this, "day20.test.txt").toIndexedSeq
  lazy val input: IndexedSeq[String] = Loader(this, "day20.txt").toIndexedSeq

  it should "solve part 1 test" in {
    part1(testInput) shouldBe 20899048083289L
  }

  it should "solve part 1" in {
    part1(input) shouldBe 13983397496713L
  }

  it should "solve part 2 test" in {
    part2(testInput) shouldBe 273
  }

  it should "solve part 2" in {
    part2(input) shouldBe 2424
  }

}
