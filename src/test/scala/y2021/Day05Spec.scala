package y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day05Spec extends AnyFlatSpec with Matchers with Day05 {
  lazy val testInput = Loader(this, "day05.test.txt").toSeq
  lazy val input = Loader(this, "day05.txt").toSeq

  "Day05" should "pass the part 1 tests" in {
    solve(testInput, (line: Line) => line.vertOrHoriz) shouldBe 5
  }

  it should "pass part 1" in {
    solve(input, (line: Line) => line.vertOrHoriz) shouldBe 4421
  }

  it should "pass part 2 test" in {
    solve(testInput, (line: Line) => line.vertOrHorizOrDiag) shouldBe 12
  }

  it should "pass part 2" in {
    solve(input, (line: Line) => line.vertOrHorizOrDiag) shouldBe 18674
  }
}
