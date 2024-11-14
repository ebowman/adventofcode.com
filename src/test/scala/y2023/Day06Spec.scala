package y2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day06Spec extends AnyFlatSpec with Matchers with Day06 {

  lazy val input: IndexedSeq[String] = Loader(this, "day06.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day06.test.txt").toIndexedSeq

  it should "solve part 1 test" in {
    solvePart1(testInput) shouldBe 288
  }

  it should "solve part 1" in {
    solvePart1(input) shouldBe 1660968
  }

  it should "solve part 2 test" in {
    solvePart2(testInput) shouldBe 71503
  }

  it should "solve part 2" in {
    solvePart2(input) shouldBe 26499773
  }
}
