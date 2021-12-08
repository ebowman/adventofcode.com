package y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day02Spec extends AnyFlatSpec with Matchers with Day02 {

  val testInstructions =
    """
      |forward 5
      |down 5
      |forward 8
      |up 3
      |down 8
      |forward 2
      |""".stripMargin.trim.split("\n").toSeq
  lazy val inputs = Loader(this, "day02.txt").toSeq

  "Simple example" should "navigate correctly in part 1" in {
    solve1(testInstructions) shouldBe 150
  }
  it should "solve part 1" in {
    solve1(inputs) shouldBe 1882980
  }
  it should "navigate correctly in part 2" in {
    solve2(testInstructions) shouldBe 900
  }
  it should "solve part 2" in {
    solve2(inputs) shouldBe 1971232560
  }
}
