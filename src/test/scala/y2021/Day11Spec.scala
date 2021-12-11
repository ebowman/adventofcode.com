package y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day11Spec extends AnyFlatSpec with Matchers with Day11 {
  lazy val testInput = Loader(this, "day11.test.txt").toSeq
  lazy val input = Loader(this, "day11.txt").toSeq

  "Day1." should "pass the part 1 tests" in {
    solve1(testInput) shouldBe 1656
  }

  it should "pass part 1" in {
    solve1(input) shouldBe 1591
  }

  it should "pass part 2 test" in {
    solve2(testInput) shouldBe 195
  }

  it should "pass part 2" in {
    solve2(input) shouldBe 314
  }
}
