package y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day01Spec extends AnyFlatSpec with Matchers with Day01 {

  lazy val inputs = Loader(this, "day01.txt").map(_.toInt).toSeq

  "Simple example" should "count correctly" in {
    val seq = Seq(199,200,208,210,200,207,240,269,260,263)
    countIncreases(seq) shouldBe 7
  }

  it should "solve the final input" in {
    countIncreases(inputs) shouldBe 1553
  }

}
