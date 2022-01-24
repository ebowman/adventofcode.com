package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day12Spec extends AnyFlatSpec with Matchers with Day12 {
  private lazy val input = util.Loader(this, "day12.txt").head

  it should "pass part 1" in {
    solve1(input) shouldBe 191164
  }
  it should "pass part 2" in {
    solve2(input) shouldBe 87842
  }
}
