package y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day04Spec extends AnyFlatSpec with Matchers with Day04 {
  lazy val testInput = Loader(this, "day04.test.txt").toSeq
  lazy val input = Loader(this, "day04.txt").toSeq

  "Day04" should "pass the part 1 tests" in {
    solve1(testInput) shouldBe 4512
  }

  it should "pass part 1" in {
    solve1(input) shouldBe 5685
  }

  it should "pass part 2 test" in {
    solve2(testInput) shouldBe 1924
  }

  it should "pass part 2" in {
    solve2(input) shouldBe 21070
  }
}
