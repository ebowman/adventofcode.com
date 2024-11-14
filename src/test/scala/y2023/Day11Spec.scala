package y2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day11Spec extends AnyFlatSpec with Matchers with Day11 {

  lazy val input: IndexedSeq[String] = Loader(this, "day11.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day11.test.txt").toIndexedSeq

  it should "solve part 1 test" in {
    solvePart1(testInput) shouldBe 374
  }

  it should "solve part 1" in {
    solvePart1(input) shouldBe 9591768
  }

  it should "solve part 2 test" in {
    solvePart2(testInput, 10) shouldBe 1030
    solvePart2(testInput, 100) shouldBe 8410
  }

  it should "solve part 2" in {
    solvePart2(input) shouldBe 746962097860L
  }
}
