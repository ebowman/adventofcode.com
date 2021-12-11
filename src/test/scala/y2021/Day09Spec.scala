package y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day09Spec extends AnyFlatSpec with Matchers with Day09 {
  lazy val testInput = Loader(this, "day09.test.txt").toSeq
  lazy val input = Loader(this, "day09.txt").toSeq

  "Day09" should "pass the part 1 tests" in {
    solve1(testInput) shouldBe 15
  }

  it should "pass part 1" in {
    solve1(input) shouldBe 554
  }

  it should "pass part 2 test" in {
    solve2(testInput) shouldBe 1134
  }

  it should "pass part 2" in {
    solve2(input) shouldBe 1017792
  }
}
