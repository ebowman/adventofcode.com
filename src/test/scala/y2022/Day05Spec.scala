package y2022

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day05Spec extends AnyFlatSpec with Matchers with Day05:

  lazy val input: IndexedSeq[String] = Loader(this, "day05.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day05.test.txt").toIndexedSeq

  it should "solve part 1 test" in:
    solvePart1(testInput) shouldBe "CMZ"

  it should "solve part 1" in:
    solvePart1(input) shouldBe "PTWLTDSJV"

  it should "solve part 2 test" in:
    solvePart2(testInput) shouldBe "MCD"

  it should "solve part 2" in:
    solvePart2(input) shouldBe "WZMFVGGZP"
