package y2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day20Spec extends AnyFlatSpec with Matchers with Day20 {

  lazy val input: IndexedSeq[String] = Loader(this, "day20.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day20.test.txt").toIndexedSeq
  lazy val testInput2: IndexedSeq[String] = Loader(this, "day20.test2.txt").toIndexedSeq

  it should "solve part 1 test" in {
    solvePart1(testInput) shouldBe 32000000
    solvePart1(testInput2) shouldBe 11687500
  }

  it should "solve part 1" in {
    solvePart1(input) shouldBe 806332748
  }

  it should "solve part 2" in {
    solvePart2(input) shouldBe 228060006554227L
  }
}
