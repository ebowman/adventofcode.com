package y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day21Spec extends AnyFlatSpec with Matchers with Day21:

  lazy val input: IndexedSeq[String] = Loader(this, "day21.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day21.test.txt").toIndexedSeq

  it should "solve part 1 test" in:
    solve(testInput, 2) shouldBe 12

  it should "solve part 1" in:
    solve(input, 5) shouldBe 167

  it should "solve part 2" in:
    solve(input, 18) shouldBe 2425195
