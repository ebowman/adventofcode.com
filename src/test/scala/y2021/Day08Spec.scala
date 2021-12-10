package y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day08Spec extends AnyFlatSpec with Matchers with Day08 {
  lazy val testInput = Loader(this, "day08.test.txt").toSeq
  lazy val input = Loader(this, "day08.txt").toSeq

  "Day08" should "pass the part 1 tests" in {
    solve1(testInput) shouldBe 26
  }

  it should "pass part 1" in {
    solve1(input) shouldBe 449
  }

  it should "pass part 2 test" in {
    solve2(testInput) shouldBe 61229
  }

  it should "pass part 2" in {
    solve2(input) shouldBe 968175
  }
}
