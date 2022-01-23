package y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*

class Day24Spec extends AnyFlatSpec with Matchers with Day24 {
  lazy val grid = util.Loader(this, "day24.txt").toSeq

  it should "solve part 1" in {
    solve1 shouldBe 412
  }
  it should "solve part 2" in {
    solve2 shouldBe 664
  }
}
