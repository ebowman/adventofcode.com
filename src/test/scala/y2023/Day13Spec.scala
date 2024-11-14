package y2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day13Spec extends AnyFlatSpec with Matchers with Day13 {

  lazy val input: IndexedSeq[String] = Loader(this, "day13.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day13.test.txt").toIndexedSeq
  lazy val testInput2: IndexedSeq[String] = Loader(this, "day13.test2.txt").toIndexedSeq

  it should "solve part 1 test" in {
    solvePart1(testInput) shouldBe 405
  }

  it should "solve part 1" in {
    solvePart1(input) shouldBe 37561
  }

  it should "solve part 2 test" in {
    solvePart2(testInput) shouldBe 400
  }

  it should "solve part 2" in {
    solvePart2(input) shouldBe 31108
  }
}
