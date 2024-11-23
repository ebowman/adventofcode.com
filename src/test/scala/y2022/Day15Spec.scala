package y2022

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day15Spec extends AnyFlatSpec with Matchers with Day15:

  lazy val input: IndexedSeq[String] = Loader(this, "day15.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day15.test.txt").toIndexedSeq

  it should "solve part 1 test" in:
    solvePart1(testInput, 10) shouldBe 26

  it should "solve part 1" in:
    solvePart1(input) shouldBe 4748135

  it should "solve part 2 test" in :
    solvePart2(testInput, 20) shouldBe 56000011

  it should "solve part 2" in:
    solvePart2(input) shouldBe 13743542639657L
