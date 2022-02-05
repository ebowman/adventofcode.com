package y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*

class Day04Spec extends AnyFlatSpec with Matchers with Day04:
  val input = util.Loader(this, "day04.txt").toSeq

  it should "pass part 1" in {
    solve1(input) shouldBe 451
  }
  it should "pass part 2" in {
    solve2(input) shouldBe 223
  }

end Day04Spec

