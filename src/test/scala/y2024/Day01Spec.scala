package y2024

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day01Spec extends AnyFlatSpec with Matchers with Day01:

  lazy val input: IndexedSeq[String] = Loader(this, "day01.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day01.test.txt").toIndexedSeq

  it should "solve part 1 test" in:
    solvePart1(testInput) shouldBe 11

  it should "solve part 1" in:
    solvePart1(input) shouldBe 1941353

  it should "solve part 2 test" in:
    solvePart2(testInput) shouldBe 31

  it should "solve part 2" in:
    solvePart2(input) shouldBe 22539317
