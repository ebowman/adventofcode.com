package y2022

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day20Spec extends AnyFlatSpec with Matchers with Day20:

  lazy val input: IndexedSeq[String] = Loader(this, "day20.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day20.test.txt").toIndexedSeq

  it should "solve part 1 test" in:
    solvePart1(testInput) shouldBe 3

  it should "solve part 1" in:
    solvePart1(input) shouldBe 2827

  it should "solve part 2 test" in:
    solvePart2(testInput) shouldBe 1623178306

  it should "solve part 2" in:
    solvePart2(input) shouldBe 7834270093909L
