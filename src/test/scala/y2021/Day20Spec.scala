package y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day20Spec extends AnyFlatSpec with Matchers with Day20 {
  private lazy val testInput = util.Loader(this, "day20.test.txt").toSeq
  private lazy val input = util.Loader(this, "day20.txt").toSeq

  it should "pass part 1 tests" in {
    solve(testInput, 2) shouldBe 35
  }

  it should "pass part 1" in {
    solve(input, 2) shouldBe 5622
  }

  it should "pass part 2 tests" in {
    solve(testInput, 50) shouldBe 3351
  }

  it should "pass part 2" in {
    solve(input, 50) shouldBe 20395
  }
}
