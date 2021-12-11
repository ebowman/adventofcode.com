package y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day10Spec extends AnyFlatSpec with Matchers with Day10 {
  lazy val testInput = Loader(this, "day10.test.txt").toSeq
  lazy val input = Loader(this, "day10.txt").toSeq

  "Day10" should "pass the part 1 tests" in {
    solve1(testInput) shouldBe 26397
  }

  it should "pass part 1" in {
    solve1(input) shouldBe 392139
  }

  it should "pass part 2 test" in {
    solve2(testInput) shouldBe 288957L
  }

  it should "pass part 2" in {
    solve2(input) shouldBe 4001832844L
  }
}
