package y2019

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day15Spec extends AnyFlatSpec with Matchers with Day15 {

  lazy val code: String = Loader.is(this, "day15.txt").head

  it should "solve part 1" in {
    part1(code) shouldBe 270
  }

  it should "solve part 2" in {
    part2(code) shouldBe 364
  }
}
