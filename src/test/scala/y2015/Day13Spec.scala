package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day13Spec extends AnyFlatSpec with Matchers with Day13 {
  private lazy val input = Loader(this, "day13.txt")
  it should "pass part 1" in {
    solve1(input) shouldBe 664
  }

  it should "pass part 2" in {
    solve2(input) shouldBe 640
  }
}
