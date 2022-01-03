package y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day02Spec extends AnyFlatSpec with Matchers with Day02 {
  private lazy val testInput = util.Loader(this, "day02.test.txt").toSeq
  private lazy val input = util.Loader(this, "day02.txt").toSeq

  it should "pass part 1 tests" in {
    solve1(testInput) shouldBe "1985"
  }

  it should "pass part 1" in {
    solve1(input) shouldBe "33444"
  }

  it should "pass part 2 tests" in {
    solve2(testInput) shouldBe "5DB3"
  }

  it should "pass part 2" in {
    solve2(input) shouldBe "446A6"
  }
}
