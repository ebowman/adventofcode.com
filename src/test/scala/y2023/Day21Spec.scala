package y2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day21Spec extends AnyFlatSpec with Matchers with Day21 {

  lazy val input: IndexedSeq[String] = Loader(this, "day21.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day21.test.txt").toIndexedSeq
  lazy val testInput2: IndexedSeq[String] = Loader(this, "day21.test2.txt").toIndexedSeq

  it should "solve part 1 test" in {
    solvePart1(testInput, 6) shouldBe 16
  }

  it should "solve part 1" in {
    solvePart1(input, 64) shouldBe 3737
  }

  it should "solve part 2 test" in {
    solvePart2(testInput, 6) shouldBe 16
    solvePart2(testInput, 10) shouldBe 50
    solvePart2(testInput, 50) shouldBe 1594
    solvePart2(testInput, 100) shouldBe 6536
    solvePart2(testInput, 500) shouldBe 167004
    solvePart2(testInput, 1000) shouldBe 668697

    // This test case is too slow
    // solvePart2(testInput, 5000) shouldBe 16733044
  }

  it should "solve part 2" in {
    solvePart2(input, 26501365) shouldBe 625382480005896L
  }
}
