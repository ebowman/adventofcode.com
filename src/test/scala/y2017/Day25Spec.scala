package y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day25Spec extends AnyFlatSpec with Matchers with Day25:

  lazy val input: IndexedSeq[String] = Loader(this, "day25.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day25.test.txt").toIndexedSeq

  it should "solve part 1 test" in:
    solvePart1(testInput) shouldBe 3

  it should "solve part 1" in:
    solvePart1(input) shouldBe 2846

