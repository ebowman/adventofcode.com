package y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day18Spec extends AnyFlatSpec with Matchers with Day18:

  lazy val input: IndexedSeq[String] = Loader(this, "day18.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day18.test.txt").toIndexedSeq
  lazy val testInput2: IndexedSeq[String] = Loader(this, "day18.test2.txt").toIndexedSeq

  it should "solve part 1 test" in:
    solvePart1(testInput) shouldBe 4

  it should "solve part 1" in:
    solvePart1(input) shouldBe 3188

  it should "solve part 2 test" in:
    solvePart2(testInput2) shouldBe 3

  it should "solve part 2" in:
    solvePart2(input) shouldBe 7112
