package y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day19Spec extends AnyFlatSpec with Matchers with Day19:

  lazy val input: IndexedSeq[String] = Loader(this, "day19.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day19.test.txt").toIndexedSeq
  lazy val testInput2: IndexedSeq[String] = Loader(this, "day19.test2.txt").toIndexedSeq

  it should "solve part 1 test" in:
    solvePart1(testInput) shouldBe "ABCDEF"

  it should "solve part 1" in:
    solvePart1(input) shouldBe "SXWAIBUZY"

  it should "solve part 2 test" in:
    solvePart2(testInput) shouldBe 38

  it should "solve part 2" in:
    solvePart2(input) shouldBe 16676
