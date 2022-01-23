package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day09Spec extends AnyFlatSpec with Matchers with Day09 {

  private lazy val input = Loader(this, "day09.txt").toSeq

  it should "pass parts 1 & 2" in {
    solve(input) shouldBe(117, 909)
  }
}
