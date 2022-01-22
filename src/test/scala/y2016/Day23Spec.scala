package y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*

class Day23Spec extends AnyFlatSpec with Matchers with Day23 {
  lazy val input: Seq[String] = util.Loader(this, "day23.txt").toSeq

  it should "solve" in {
    solve1(input, 7) shouldBe 12663
    solve1(input, 12) shouldBe 479009223
  }
}
