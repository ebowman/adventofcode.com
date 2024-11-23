package y2022

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day02Spec extends AnyFlatSpec with Matchers with Day02:

  lazy val input: IndexedSeq[String] = Loader(this, "day02.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day02.test.txt").toIndexedSeq

  it should "solve part 1 test" in:
    solvePart1(testInput) shouldBe 15

  it should "solve part 1" in:
    solvePart1(input) shouldBe 10_718

  it should "solve part 2 test" in:
    solvePart2(testInput) shouldBe 12

  it should "solve part 2" in:
    solvePart2(input) shouldBe 14_652
