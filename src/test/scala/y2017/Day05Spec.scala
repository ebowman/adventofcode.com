package y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*

class Day05Spec extends AnyFlatSpec with Matchers with Day05 :
  val input = util.Loader(this, "day05.txt").toSeq

  it should "pass part 1" in {
    solve1(input) shouldBe 374269
  }
  it should "pass part 2" in {
    solve2(input) shouldBe 27720699
  }

end Day05Spec

