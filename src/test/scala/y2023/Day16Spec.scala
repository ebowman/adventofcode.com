package y2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day16Spec extends AnyFlatSpec with Matchers with Day16 {

  lazy val input: IndexedSeq[String] = Loader(this, "day16.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day16.test.txt").toIndexedSeq
  lazy val testInput2: IndexedSeq[String] = Loader(this, "day16.test2.txt").toIndexedSeq

  it should "solve part 1 test" in {
    solvePart1(testInput) shouldBe 46
  }

  it should "solve part 1" in {
    solvePart1(input) shouldBe 7046
  }

  it should "solve part 2 test" in {
    solvePart2(testInput) shouldBe 51
  }

  it should "solve part 2" in {
    solvePart2(input) shouldBe 7313
  }
}
