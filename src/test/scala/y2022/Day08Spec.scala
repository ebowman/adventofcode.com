package y2022

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day08Spec extends AnyFlatSpec with Matchers with Day08:

  lazy val input: IndexedSeq[String] = Loader(this, "day08.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day08.test.txt").toIndexedSeq

  it should "solve part 1 test" in:
    solvePart1(testInput) shouldBe 21

  it should "solve part 1" in:
    solvePart1(input) shouldBe 1690

  it should "solve part 2 test" in:
    solvePart2(testInput) shouldBe 8

  it should "solve part 2" in:
    solvePart2(input) shouldBe 535680
