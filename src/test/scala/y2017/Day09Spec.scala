package y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day09Spec extends AnyFlatSpec with Matchers with Day09:

  lazy val input: IndexedSeq[String] = Loader(this, "day09.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day09.test.txt").toIndexedSeq
  lazy val testInput2: IndexedSeq[String] = Loader(this, "day09.test2.txt").toIndexedSeq

  it should "solve part 1 test" in:
    solvePart1("{}") shouldBe 1
    solvePart1("{{{}}}") shouldBe 6
    solvePart1("{{},{}}") shouldBe 5
    solvePart1("{{{},{},{{}}}}") shouldBe 16
    solvePart1("{<a>,<a>,<a>,<a>}") shouldBe 1
    solvePart1("{{<ab>},{<ab>},{<ab>},{<ab>}}") shouldBe 9
    solvePart1("{{<!!>},{<!!>},{<!!>},{<!!>}}") shouldBe 9
    solvePart1("{{<a!>},{<a!>},{<a!>},{<ab>}}") shouldBe 3

  it should "solve part 1" in:
    solvePart1(input.head) shouldBe 9251

  it should "solve part 2 test" in:
    solvePart2("<>") shouldBe 0
    solvePart2("<random characters>") shouldBe 17
    solvePart2("<<<<>") shouldBe 3
    solvePart2("<{!>}>") shouldBe 2
    solvePart2("<!!>") shouldBe 0
    solvePart2("<!!!>>") shouldBe 0
    solvePart2("<{o\"i!a,<{i<a>") shouldBe 10

  it should "solve part 2" in:
    solvePart2(input.head) shouldBe 4322
