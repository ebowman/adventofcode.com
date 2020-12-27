package y2019

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day01Spec extends AnyFlatSpec with Matchers with Day01 {

  lazy val input: IndexedSeq[String] = Loader.is(this, "day01.txt")

  it should "solve part 1 test" in {
    fuel(12)  shouldBe 2
    fuel(14)  shouldBe 2
    fuel(1969) shouldBe 654
    fuel(100756) shouldBe 33583
  }

  it should "solve part 1" in {
    part1(input) shouldBe 3345909
  }

  it should "solve part 2 test" in {
    fuel2()(14) shouldBe 2
    fuel2()(1969) shouldBe 966
    fuel2()(100756) shouldBe 50346
  }

  it should "solve part 2" in {
    part2(input) shouldBe 5015983
  }
}
