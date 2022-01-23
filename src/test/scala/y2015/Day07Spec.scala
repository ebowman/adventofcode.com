package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day07Spec extends AnyFlatSpec with Matchers with Day07 {

  private lazy val input = util.Loader(this, "day07.txt").toSeq

  it should "pass part 1" in {
    compile(input)("a").voltage shouldBe 956
  }

  it should "pass part 2" in {
    compile(input :+ "956 -> b")("a").voltage shouldBe 40149
  }
}
