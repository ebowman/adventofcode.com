package y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day22Spec extends AnyFlatSpec with Matchers with Day22 {
  private lazy val testSimpleInput = util.Loader(this, "day22.test2.txt").toSeq
  private lazy val testInput = util.Loader(this, "day22.test.txt").toSeq
  private lazy val test3Input = util.Loader(this, "day22.test3.txt").toSeq
  private lazy val input = util.Loader(this, "day22.txt").toSeq

  it should "pass part 1 tests" in {
    solve1(testInput) shouldBe 590784
  }

  it should "pass part 1" in {
    solve1(input) shouldBe 596989
  }

  it should "pass part 2 simple tests" in {
    solve2(testSimpleInput) shouldBe 7957L
  }

  it should "pass part 2 tests" in {
    solve2(test3Input) shouldBe 2758514936282235L
  }

  it should "pass part 2" in {
    solve2(input) shouldBe 1160011199157381L
  }
}
