package y2022

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day11Spec extends AnyFlatSpec with Matchers with Day11:

  lazy val input: IndexedSeq[String] = Loader(this, "day11.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day11.test.txt").toIndexedSeq

  it should "solve part 1 test" in:
    solvePart1(testInput) shouldBe 10605

  it should "solve part 1" in:
    solvePart1(input) shouldBe 107822

  it should "solve part 2 test" in:
    solvePart2(testInput) shouldBe 2713310158L

  it should "solve part 2" in:
    solvePart2(input) shouldBe 27267163742L
