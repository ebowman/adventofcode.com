package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*

class Day03Spec extends AnyFlatSpec with Matchers with Day03 {
  private lazy val input = util.Loader(this, "day03.txt").head

  it should "solve part 1" in {
    solve1(input) shouldBe 2081
  }
  it should "solve part 2" in {
    solve2(input) shouldBe 2341
  }
}
