package y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day09Spec extends AnyFlatSpec with Matchers with Day09 {
  private lazy val input = util.Loader(this, "day09.txt").toSeq.head
  it should "pass part 1" in {
    solve1(input) shouldBe 123908
  }
  it should "pass part 2" in {
    solve2(input) shouldBe 10755693147L
  }
}
