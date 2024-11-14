package y2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day09Spec extends AnyFlatSpec with Matchers with Day09 {

  lazy val input: IndexedSeq[String] = Loader(this, "day09.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day09.test.txt").toIndexedSeq
  lazy val testInput2: IndexedSeq[String] = Loader(this, "day09.test2.txt").toIndexedSeq

  it should "solve part 1 test" in {
    solvePart1(testInput) shouldBe 114
  }

  it should "solve part 1" in {
    solvePart1(input) shouldBe 1972648895
  }

  it should "solve part 2 test" in {
    solvePart2(testInput) shouldBe 2
  }

  it should "solve part 2" in {
    solvePart2(input) shouldBe 919
  }
}
