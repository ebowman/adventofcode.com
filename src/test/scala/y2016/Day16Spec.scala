package y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day16Spec extends AnyFlatSpec with Matchers with Day16 {
  it should "solve part 1" in {
    solve1("11101000110010100", 272) shouldBe "10100101010101101"
  }
  it should "solve part 2" in {
    solve1("11101000110010100", 35651584) shouldBe "01100001101101001"
  }
}
