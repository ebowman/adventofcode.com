package y2019

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day13Spec extends AnyFlatSpec with Matchers with Day13 {

  lazy val input: String = Loader.is(this, "day13.txt").head

  it should "solve part 1" in {
    part1(input) shouldBe 361
  }

  it should "solve part 2" in {
    part2(input) shouldBe 17590
  }
}
