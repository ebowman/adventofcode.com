package y2022

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day12Spec extends AnyFlatSpec with Matchers with Day12:

  lazy val input: IndexedSeq[String] = Loader(this, "day12.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day12.test.txt").toIndexedSeq

  it should "solve part 1 test" in:
    solvePart1(testInput) shouldBe 31

  it should "solve part 1" in:
    solvePart1(input) shouldBe 339

  it should "solve part 2 test" in:
    solvePart2(testInput) shouldBe 29

  it should "solve part 2" in:
    solvePart2(input) shouldBe 332
