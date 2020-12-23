package y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day23Spec extends AnyFlatSpec with Matchers with Day23 {

  it should "solve part 1 test" in {
    part1(389125467) shouldBe 67384529
  }

  it should "solve part 1" in {
    part1(157623984) shouldBe 58427369
  }

  it should "solve part 2" in {
    part2(157623984) shouldBe 111057672960L
  }
}
