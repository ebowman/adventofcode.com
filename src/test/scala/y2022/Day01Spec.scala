package y2022

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day01Spec extends AnyFlatSpec with Matchers with Day01:

  lazy val input: IndexedSeq[String] = Loader(this, "day01.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day01.test.txt").toIndexedSeq

  it should "solve part 1 test" in:
    solvePart1(testInput) shouldBe 24_000

  it should "solve part 1" in:
    solvePart1(input) shouldBe 68_467

  it should "solve part 2 test" in:
    solvePart2(testInput) shouldBe 45_000

  it should "solve part 2" in:
    solvePart2(input) shouldBe 203_420
