package y2019

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day05Spec extends AnyFlatSpec with Matchers with Day05 {
  lazy val input: String = Loader.is(this, "day05.txt").head

  it should "solve part 1" in {
    part1(input) shouldBe 13818007
  }

  it should "solve part 2" in {
    part2(input) shouldBe 3176266
  }
}
