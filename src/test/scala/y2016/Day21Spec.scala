package y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*

class Day21Spec extends AnyFlatSpec with Matchers with Day21 {
  lazy val input: Seq[String] = util.Loader(this, "day21.txt").toSeq

  it should "solve" in {
    solve1(input, "abcdefgh") shouldBe "gbhafcde"
  }
  it should "solve2" in {
    solve2(input, "fbgdceah") shouldBe "bcfaegdh"
  }
}
