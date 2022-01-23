package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*

class Day01Spec extends AnyFlatSpec with Matchers with Day01 {
  lazy val input = util.Loader(this, "day01.txt").head

  it should "solve part 1" in {
    solve1(input) shouldBe 138
  }
  it should "solve part 2" in {
    solve2(input) shouldBe 1771
  }
}
