package y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day10Spec extends AnyFlatSpec with Matchers with Day10 {
  private lazy val input = util.Loader(this, "day10.txt").toSeq
  private lazy val testInput = util.Loader(this, "day10.test.txt").toSeq

  it should "pass tests" in {
    solve(testInput, 2, 5) shouldBe(2, 30)
  }

  it should "pass parts 1 & 2" in {
    solve(input, 17, 61) shouldBe(113, 12803)
  }
}
