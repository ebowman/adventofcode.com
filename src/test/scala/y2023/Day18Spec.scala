package y2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day18Spec extends AnyFlatSpec with Matchers with Day18 {

  lazy val input: IndexedSeq[String] = Loader(this, "day18.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day18.test.txt").toIndexedSeq
  lazy val testInput2: IndexedSeq[String] = Loader(this, "day18.test2.txt").toIndexedSeq

  it should "solve part 1 test" in {
    solvePart1(testInput) shouldBe 62
  }

  it should "solve part 1" in {
    solvePart1(input) shouldBe 36725
  }

  it should "solve part 2 test" in {
    solvePart2(testInput) shouldBe 952408144115L
  }

  it should "solve part 2" in {
    solvePart2(input) shouldBe 97874103749720L
  }
}
