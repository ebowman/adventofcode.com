package y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day25Spec extends AnyFlatSpec with Matchers with Day25 {
  private lazy val testInput = util.Loader(this, "day25.test.txt").toSeq
  private lazy val input = util.Loader(this, "day25.txt").toSeq

  it should "pass part 1 tests" in {
    solve1(testInput) shouldBe 58
  }

  it should "pass part 1" in {
    solve1(input) shouldBe 414
  }
}
