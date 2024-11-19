package y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day11Spec extends AnyFlatSpec with Matchers with Day11:

  lazy val input: IndexedSeq[String] = Loader(this, "day11.txt").toIndexedSeq

  it should "solve part 1 test" in:
    solvePart1("ne,ne,ne") shouldBe 3
    solvePart1("ne,ne,sw,sw") shouldBe 0
    solvePart1("ne,ne,s,s") shouldBe 2
    solvePart1("se,sw,se,sw,sw") shouldBe 3

  it should "solve part 1" in:
    solvePart1(input.head) shouldBe 773

  it should "solve part 2" in:
    solvePart2(input.head) shouldBe 1560
