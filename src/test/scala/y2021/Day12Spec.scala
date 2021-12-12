package y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day12Spec extends AnyFlatSpec with Matchers with Day12 {
  lazy val testInput = Loader(this, "day12.test.txt").toSeq
  lazy val testInput2 = Loader(this, "day12.test.2.txt").toSeq
  lazy val input = Loader(this, "day12.txt").toSeq

  "Day12" should "pass the part 1 tests" in {
    solve1(testInput) shouldBe 19
  }

  it should "pass the second part 1 tests" in {
    solve1(testInput2) shouldBe 226
  }

  it should "pass part 1" in {
    solve1(input) shouldBe 4773
  }

  it should "pass part 2 test" in {
    solve2(testInput) shouldBe 103
  }

  it should "pass part the second 2 test" in {
    solve2(testInput2) shouldBe 3509
  }

  it should "pass part 2" in {
    solve2(input) shouldBe 116985
  }
}
