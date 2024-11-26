package y2022

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day09Spec extends AnyFlatSpec with Matchers with Day09:

  lazy val input: IndexedSeq[String] = Loader(this, "day09.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day09.test.txt").toIndexedSeq
  lazy val testInput2: IndexedSeq[String] = Loader(this, "day09.test2.txt").toIndexedSeq

  it should "solve part 1 test" in:
    solve(testInput, 2) shouldBe 13

  it should "solve part 1" in:
    solve(input, 2) shouldBe 5695

  it should "solve part 2 test" in:
    solve(testInput2, 10) shouldBe 36

  it should "solve part 2" in:
    solve(input, 10) shouldBe 2434
