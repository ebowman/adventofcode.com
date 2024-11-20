package y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day20Spec extends AnyFlatSpec with Matchers with Day20:

  lazy val input: IndexedSeq[String] = Loader(this, "day20.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day20.test.txt").toIndexedSeq

  it should "solve part 1 test" in:
    solvePart1(testInput) shouldBe 0

  it should "solve part 1" in:
    solvePart1(input) shouldBe 243

  it should "solve part 2 test" in:
    solvePart2(testInput) shouldBe 2

  it should "solve part 2" in:
    solvePart2(input) shouldBe 648
