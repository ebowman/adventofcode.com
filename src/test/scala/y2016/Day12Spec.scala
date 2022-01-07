package y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day12Spec extends AnyFlatSpec with Matchers with Day12 {
  private lazy val input = util.Loader(this, "day12.txt").toSeq

  it should "solve part 1" in {
    solve(input) shouldBe 318007
  }
  it should "solve part 2" in {
    solve(input, Map("c" -> 1)) shouldBe 9227661
  }
}
