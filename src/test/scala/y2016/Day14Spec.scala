package y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day14Spec extends AnyFlatSpec with Matchers with Day14 {
  override val salt = "jlmsuwbz"
  it should "solve part 1" in {
    solve(false) shouldBe 35186
  }
  it should "solve part 2" in {
    solve(true) shouldBe  22429
  }
}
