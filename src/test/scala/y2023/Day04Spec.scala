package y2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day04Spec extends AnyFlatSpec with Matchers with Day04 {

  lazy val input: IndexedSeq[String] = Loader(this, "day04.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day04.test.txt").toIndexedSeq

  it should "solve part 1 test" in {
    solvePart1(testInput) shouldBe 13
  }

  it should "solve part 1" in {
    solvePart1(input) shouldBe 21919
  }

  it should "solve part 2 test" in {
    solvePart2(testInput) shouldBe 30
  }

  it should "solve part 2" in {
    solvePart2(input) shouldBe 9881048
  }
}
