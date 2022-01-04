package y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day06Spec extends AnyFlatSpec with Matchers with Day06 {
  private lazy val input = util.Loader(this, "day06.txt").toSeq

  it should "pass part 1" in {
    solve(input, _.maxBy(_._2)._1) shouldBe "bjosfbce"
  }
  it should "pass part 2" in {
    solve(input, _.minBy(_._2)._1) shouldBe "veqfxzfx"
  }
}
