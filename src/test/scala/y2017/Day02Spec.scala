package y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*

class Day02Spec extends AnyFlatSpec with Matchers with Day02:
  private lazy val input = util.Loader(this, "day02.txt").toSeq

  it should "pass part 1" in {
    solve1(input) shouldBe 41887
  }
  it should "pass part 2" in {
    solve2(input) shouldBe 226
  }

end Day02Spec

