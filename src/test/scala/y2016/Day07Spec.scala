package y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day07Spec extends AnyFlatSpec with Matchers with Day07 {
  private lazy val input = util.Loader(this, "day07.txt").toSeq

  it should "pass part 1" in {
    solve1(input) shouldBe 118
  }

  it should "pass part 2" in {
    solve2(input) shouldBe 260
  }
}
