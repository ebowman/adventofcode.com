package y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day06Spec extends AnyFlatSpec with Matchers with Day06:

  lazy val input: IndexedSeq[String] = Loader(this, "day06.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day06.test.txt").toIndexedSeq
  lazy val testInput2: IndexedSeq[String] = Loader(this, "day06.test2.txt").toIndexedSeq

  it should "solve part 1 test" in:
    solvePart1(testInput) shouldBe 5

  it should "solve part 1" in:
    solvePart1(input) shouldBe 4074

  it should "solve part 2 test" in:
    solvePart2(testInput) shouldBe 4

  it should "solve part 2" in:
    solvePart2(input) shouldBe 2793