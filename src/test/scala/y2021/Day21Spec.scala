package y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day21Spec extends AnyFlatSpec with Matchers with Day21 {
  private lazy val testInput = util.Loader(this, "day21.test.txt").toSeq
  private lazy val input = util.Loader(this, "day21.txt").toSeq

  it should "pass part 1 tests" in {
    solve1(testInput) shouldBe 739785
  }

  it should "pass part 1" in {
    solve1(input) shouldBe 752247
  }

  it should "pass part 2 tests" in {
    solve2(testInput) shouldBe 444356092776315L
  }

  it should "pass part 2" in {
    solve2(input) shouldBe 221109915584112L
  }
}
