package y2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day15Spec extends AnyFlatSpec with Matchers with Day15 {

  lazy val input: IndexedSeq[String] = Loader(this, "day15.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day15.test.txt").toIndexedSeq
  lazy val testInput2: IndexedSeq[String] = Loader(this, "day15.test2.txt").toIndexedSeq

  it should "solve part 1 test" in {
    solvePart1(testInput.head) shouldBe 1320
  }

  it should "solve part 1" in {
    solvePart1(input.head) shouldBe 511343
  }

  it should "solve part 2 test" in {
    solvePart2(testInput.head) shouldBe 145
  }

  it should "solve part 2" in {
    println(solvePart2(input.head))
  }
}
