package y2019

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day14Spec extends AnyFlatSpec with Matchers with Day14 {

  lazy val input: IndexedSeq[String] = Loader.is(this, "day14.txt")

  it should "solve part 1" in {
    part1(input) shouldBe 1037742
  }

  it should "solve part 2" in {
    part2(input) shouldBe 1572358
  }
}
