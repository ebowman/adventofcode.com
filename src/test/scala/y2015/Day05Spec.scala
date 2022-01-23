package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*

class Day05Spec extends AnyFlatSpec with Matchers with Day05 {
  private val input = util.Loader(this, "day05.txt").toSeq
  it should "solve part 1" in {
    input.count(isNice) shouldBe 238
  }
  it should "solve part 2" in {
    input.count(isNice2) shouldBe 69
  }
}
