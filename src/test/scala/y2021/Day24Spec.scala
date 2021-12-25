package y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day24Spec extends AnyFlatSpec with Matchers with Day24 {
  private lazy val input = util.Loader(this, "day24.txt").toSeq

  it should "pass parts 1 & 2" in {
    solve(input) shouldBe(91411143612181L, 92967699949891L)
  }
}
