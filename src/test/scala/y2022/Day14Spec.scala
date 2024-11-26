package y2022

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day14Spec extends AnyFlatSpec with Matchers with Day14:

  lazy val input: IndexedSeq[String] = Loader(this, "day14.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day14.test.txt").toIndexedSeq

  it should "solve part 1 test" in:
    solve(testInput, part2 = false) shouldBe 24

  it should "solve part 1" in:
    solve(input, part2 = false) shouldBe 1072

  it should "solve part 2 test" in:
    solve(testInput, part2 = true) shouldBe 93

  it should "solve part 2" in:
    solve(input, part2 = true) shouldBe 24659
