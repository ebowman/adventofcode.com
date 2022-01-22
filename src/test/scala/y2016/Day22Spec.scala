package y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*

class Day22Spec extends AnyFlatSpec with Matchers with Day22 {
  lazy val input: Seq[String] = util.Loader(this, "day22.txt").toSeq

  it should "solve" in {
    solve1(input) shouldBe 990
  }

  it should "solve part 2" in {
    solve2(input) shouldBe 218
  }
}
