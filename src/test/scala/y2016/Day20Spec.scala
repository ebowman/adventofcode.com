package y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day20Spec extends AnyFlatSpec with Matchers with Day20 {
  lazy val input: Seq[String] = util.Loader(this, "day20.txt").toSeq

  it should "solve" in {
    solve1(input) shouldBe (14975795, 101)
  }
}
