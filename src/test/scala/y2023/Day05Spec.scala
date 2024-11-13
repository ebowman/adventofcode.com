package y2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*
import util.Loader

class Day05Spec extends AnyFlatSpec with Matchers with Day05 {

  lazy val input: IndexedSeq[String] = Loader(this, "day05.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day05.test.txt").toIndexedSeq
  lazy val testInput2: IndexedSeq[String] = Loader(this, "day05.test2.txt").toIndexedSeq

  it should "solve part 1 test" in {
    solvePart1(testInput) shouldBe 35
  }

  it should "solve part 1" in {
    solvePart1(input) shouldBe 318728750
  }

  it should "solve part 2 test" in {
    solvePart2(testInput) shouldBe 46
  }

  it should "solve part 2" in {
    solvePart2(input) shouldBe 37384986
  }
}
