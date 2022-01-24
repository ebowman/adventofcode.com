package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day14Spec extends AnyFlatSpec with Matchers with Day14 {
  private lazy val input = util.Loader(this, "day14.txt")

  it should "pass part 1" in {
    solve1() shouldBe 2660
  }
  it should "pass part 2" in {
    solve2(input) shouldBe 1256
  }
}
