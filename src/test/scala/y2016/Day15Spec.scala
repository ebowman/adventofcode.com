package y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day15Spec extends AnyFlatSpec with Matchers with Day15 {
  lazy val input = util.Loader(this, "day15.txt").toSeq

  it should "solve part 1" in {
    solve1(input) shouldBe 317371
  }
  it should "solve part 2" in {

    solve1(input :+ "Disc #7 has 11 positions; at time=0, it is at position 0.") shouldBe 2080951
  }
}
