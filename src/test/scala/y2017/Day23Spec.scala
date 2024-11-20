package y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day23Spec extends AnyFlatSpec with Matchers with Day23:

  lazy val input: IndexedSeq[String] = Loader(this, "day23.txt").toIndexedSeq

  it should "solve part 1" in:
    solvePart1(input) shouldBe 8281

  it should "solve part 2" in:
    solvePart2(input) shouldBe 911
