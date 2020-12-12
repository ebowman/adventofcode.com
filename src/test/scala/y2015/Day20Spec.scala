package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day20Spec extends AnyFlatSpec with Matchers with Day20 {
  "Elves" should "win" in {
    val values = Seq(10, 30, 40, 70, 60, 120, 80, 150, 130, 180)
    values.zip(houses).forall { case (v, h) => v == h.presents } shouldBe true
  }
  it should "pass the first test" in {
    solve(36000000) shouldBe 831600
  }
  it should "pass the second test" in {
    solve2(36000000) shouldBe 884520
  }
}

