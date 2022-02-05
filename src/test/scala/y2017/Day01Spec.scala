package y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day01Spec extends AnyFlatSpec with Matchers with Day01:
  private lazy val input = util.Loader(this, "day01.txt").head

  it should "pass part 1" in {
    solve1(input) shouldBe 1047
  }

  it should "pass part 2" in {
    solve2(input) shouldBe 982
  }
end Day01Spec

