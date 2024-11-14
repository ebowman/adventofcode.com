package y2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day08Spec extends AnyFlatSpec with Matchers with Day08 {

  lazy val input: IndexedSeq[String] = Loader(this, "day08.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day08.test.txt").toIndexedSeq
  lazy val testInput2: IndexedSeq[String] = Loader(this, "day08.test2.txt").toIndexedSeq
  lazy val testInput3: IndexedSeq[String] = Loader(this, "day08.test3.txt").toIndexedSeq

  it should "solve part 1 test" in {
    solvePart1(testInput) shouldBe 2
    solvePart1(testInput2) shouldBe 6
  }

  it should "solve part 1" in {
    solvePart1(input) shouldBe 16271
  }

  it should "solve part 2 test" in {
    solvePart2(testInput3) shouldBe 6
  }

  it should "solve part 2" in {
    solvePart2(input) shouldBe 14265111103729L
  }
}
