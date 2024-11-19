package y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day17Spec extends AnyFlatSpec with Matchers with Day17:

  lazy val input: IndexedSeq[String] = Loader(this, "day17.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day17.test.txt").toIndexedSeq
  lazy val testInput2: IndexedSeq[String] = Loader(this, "day17.test2.txt").toIndexedSeq

  it should "solve part 1 test" in:
    solvePart1(3) shouldBe 638

  it should "solve part 1" in:
    solvePart1(349) shouldBe 640

  it should "solve part 2" in:
    solvePart2(349) shouldBe 47949463
