package y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day17Spec extends AnyFlatSpec with Matchers with Day17 {
  it should "solve part 1" in {
    solve1("hhhxzeay") shouldBe "DDRUDLRRRD"
  }
  it should "solve part 2" in {
    solve2("hhhxzeay") shouldBe 398
  }
}
