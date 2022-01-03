package y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day04Spec extends AnyFlatSpec with Matchers with Day04 {
  private lazy val input = util.Loader(this, "day04.txt").toSeq

  it should "pass part 1" in {
    solve1(input) shouldBe 185371
  }

  it should "pass part 2" in {
    solve2(input) shouldBe Some(984)
  }
}
