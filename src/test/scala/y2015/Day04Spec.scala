package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*

class Day04Spec extends AnyFlatSpec with Matchers with Day04 {
  private val secret = "ckczppom"
  it should "solve part 1" in {
    solve(secret, 5) shouldBe 117946
  }
  it should "solve part 2" in {
    solve(secret, 6) shouldBe 3938038
  }
}
