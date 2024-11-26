package y2022

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day22Spec extends AnyFlatSpec with Matchers with Day22:

  lazy val input: IndexedSeq[String] = Loader(this, "day22.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day22.test.txt").toIndexedSeq

  it should "solve part 1 test" in:
    solvePart1(testInput) shouldBe 6032

  it should "solve part 1" in:
    solvePart1(input) shouldBe 181128

  it should "solve part 2 test" in:
    solvePart2(testInput) shouldBe 5031

  it should "solve part 2" in:
    solvePart2(input) shouldBe 52311
