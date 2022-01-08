package y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day13Spec extends AnyFlatSpec with Matchers with Day13 {
  val input = 1364
  it should "solve part 1" in {
    solve1(input, (31, 39)) shouldBe 86
  }
  it should "solve part 2" in {
    solve2(input) shouldBe 127
  }
}
