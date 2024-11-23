package y2022

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day04Spec extends AnyFlatSpec with Matchers with Day04:

  lazy val input: IndexedSeq[String] = Loader(this, "day04.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day04.test.txt").toIndexedSeq

  it should "solve part 1 test" in:
    solvePart1(testInput) shouldBe 2

  it should "solve part 1" in:
    solvePart1(input) shouldBe 496

  it should "solve part 2 test" in:
    solvePart2(testInput) shouldBe 4

  it should "solve part 2" in:
    solvePart2(input) shouldBe 847
