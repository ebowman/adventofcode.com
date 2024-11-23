package y2022

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day21Spec extends AnyFlatSpec with Matchers with Day21:

  lazy val input: IndexedSeq[String] = Loader(this, "day21.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day21.test.txt").toIndexedSeq
  lazy val testInput2: IndexedSeq[String] = Loader(this, "day21.test2.txt").toIndexedSeq

  it should "solve part 1 test" in:
    solvePart1(testInput) shouldBe 152

  it should "solve part 1" in:
    solvePart1(input) shouldBe 168502451381566L

  it should "solve part 2 test" in:
    solvePart2(testInput) shouldBe 301

  it should "solve part 2" in:
    solvePart2(input) shouldBe 3343167719435L
