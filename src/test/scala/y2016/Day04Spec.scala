package y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day03Spec extends AnyFlatSpec with Matchers with Day03 {
  private lazy val input = util.Loader(this, "day03.txt").toSeq

  it should "pass part 1" in {
    solve1(input) shouldBe 983
  }

  it should "pass part 2" in {
    solve2(input) shouldBe 1836
  }
}
