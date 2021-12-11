package y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day03Spec extends AnyFlatSpec with Matchers with Day03 {

  val testInput =
    """
      |00100
      |11110
      |10110
      |10111
      |10101
      |01111
      |00111
      |11100
      |10000
      |11001
      |00010
      |01010""".stripMargin.trim.split("\n").toSeq

  lazy val inputs = Loader(this, "day03.txt").toSeq

  "Simple example" should "navigate correctly in part 1" in {
    solve1(testInput) shouldBe 198
  }
  it should "solve part 1" in {
    solve1(inputs) shouldBe 1082324
  }
  it should "test part 2" in {
    (solve2(testInput, '1') * solve2(testInput, '0')) shouldBe 230
  }

  it should "solve part 2" in {
    solve2(inputs, '1') * solve2(inputs, '0') shouldBe 1353024
  }
}
