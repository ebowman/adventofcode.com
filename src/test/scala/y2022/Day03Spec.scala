package y2022

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day03Spec extends AnyFlatSpec with Matchers with Day03:

  lazy val input: IndexedSeq[String] = Loader(this, "day03.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day03.test.txt").toIndexedSeq
  lazy val testInput2: IndexedSeq[String] = Loader(this, "day03.test2.txt").toIndexedSeq

  it should "solve part 1 test" in:
    solvePart1(testInput) shouldBe 157

  it should "solve part 1" in:
    solvePart1(input) shouldBe 7_917

  it should "solve part 2 test" in:
    solvePart2(testInput) shouldBe 70

  it should "solve part 2" in:
    solvePart2(input) shouldBe 2_585