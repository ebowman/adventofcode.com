package y2019

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day04Spec extends AnyFlatSpec with Matchers with Day04 {
  it should "solve part 1" in {
    part1(248345, 746315) shouldBe 1019
  }
  it should "solve part 2" in {
    part2(248345, 746315) shouldBe 660
  }
}
