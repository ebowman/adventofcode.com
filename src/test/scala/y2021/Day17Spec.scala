package y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day17Spec extends AnyFlatSpec with Matchers with Day17 {
  lazy val testInput = "target area: x=20..30, y=-10..-5"
  lazy val input = "target area: x=175..227, y=-134..-79"

  "Day17" should "pass part 1 & 2 tests" in {
    solve1(testInput) shouldBe(45, 112)
  }

  it should "pass part 1 & 2" in {
    solve1(input) shouldBe(8911, 4748)
  }
}
