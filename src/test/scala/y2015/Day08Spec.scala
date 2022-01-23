package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day08Spec extends AnyFlatSpec with Matchers with Day08 {

  private lazy val input = util.Loader(this, "day08.txt").toSeq

  it should "pass part 1" in {
    input.map(escape).map((a, b) => a - b).sum shouldBe 1333
  }

  it should "pass part 2" in {
    val lines = Loader(this, "day08.txt")
    lines.map(encode).map((a, b) => b - a).sum shouldBe 2046
  }
}
