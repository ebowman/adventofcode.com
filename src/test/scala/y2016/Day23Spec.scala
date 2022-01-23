package y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*

class Day23Spec extends AnyFlatSpec with Matchers with Day23 {
  lazy val input: Seq[String] = util.Loader(this, "day23.txt").toSeq

  it should "solve part 1" in {
    solve(input, 7) shouldBe 12663
  }
  
  it should "solve part 2" in {
    solve(input, 12) shouldBe 479009223
  }
}
