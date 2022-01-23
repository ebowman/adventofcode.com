package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day11Spec extends AnyFlatSpec with Matchers with Day11 {
  it should "solve part 1" in {
    solve("hxbxwxba")shouldBe "hxbxxyzz"
  }
  it should "solve part 2" in {
    solve("hxbxxyzz")shouldBe "hxcaabcc"
  }
}
