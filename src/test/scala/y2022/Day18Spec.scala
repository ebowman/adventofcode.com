package y2022

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day18Spec extends AnyFlatSpec with Matchers with Day18:

  lazy val input: IndexedSeq[String] = Loader(this, "day18.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day18.test.txt").toIndexedSeq

  it should "solve part 1 test" in:
    solvePart1(testInput) shouldBe 64

  it should "solve part 1" in:
    solvePart1(input) shouldBe 3586

  it should "solve part 2 test" in:
    solvePart2(testInput) shouldBe 58

  it should "solve part 2" in:
    solvePart2(input) shouldBe 2072
