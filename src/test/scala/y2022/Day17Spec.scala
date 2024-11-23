package y2022

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day17Spec extends AnyFlatSpec with Matchers with Day17:

  lazy val input: IndexedSeq[String] = Loader(this, "day17.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day17.test.txt").toIndexedSeq

  it should "solve part 1 test" in:
    solvePart1(testInput) shouldBe 3068

  it should "solve part 1" in:
    solvePart1(input) shouldBe 3181

  it should "solve part 2 test" in:
    solvePart2(testInput) shouldBe 1514285714288L

  it should "solve part 2" in:
    solvePart2(input) shouldBe 1570434782634L
