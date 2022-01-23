package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day06Spec extends AnyFlatSpec with Matchers with Day06 {

  private lazy val input = util.Loader(this, "day06.txt").toSeq

  it should "pass part 1" in {
    solve1(input) shouldBe 377891
  }

  it should "pass part 2" in {
    solve2(input) shouldBe 14110788
  }
}
