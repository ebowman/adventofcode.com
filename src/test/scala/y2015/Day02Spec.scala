package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*

class Day02Spec extends AnyFlatSpec with Matchers with Day02 {
  lazy val input = util.Loader(this, "day02.txt")

  it should "solve part 1" in {
    solve1(input) shouldBe 1588178
  }
  it should "solve part 2" in {
    solve2(input) shouldBe 3783758
  }
}
