package y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day15Spec extends AnyFlatSpec with Matchers with Day15 {
  lazy val testInput = Loader(this, "day15.test.txt").toSeq
  lazy val input = Loader(this, "day15.txt").toSeq

  "Day15" should "pass the part 1 tests" in {
    Puzzle(testInput).solve shouldBe 40
  }

  it should "pass part 1" in {
    Puzzle(input).solve shouldBe 741
  }

  it should "pass part 2 test" in {
    Puzzle(testInput).expand.solve shouldBe 315
  }

  it should "pass part 2" in {
    Puzzle(input).expand.solve shouldBe 2976
  }
}
