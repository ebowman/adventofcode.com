package y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*

class Day03Spec extends AnyFlatSpec with Matchers with Day03:
  it should "pass part 1" in {
    solve1(265149) shouldBe 438
  }
  it should "pass part 2" in {
   solve2(265149) shouldBe 266330
  }

end Day03Spec

