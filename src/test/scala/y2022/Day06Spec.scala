package y2022

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*
import util.Loader

class Day06Spec extends AnyFlatSpec with Matchers with Day06:

  lazy val input: IndexedSeq[String] = Loader(this, "day06.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day06.test.txt").toIndexedSeq

  it should "solve part 1 test" in :
    solvePart1(testInput) shouldBe 7

  it should "solve part 1" in :
    solvePart1(input) shouldBe 1909

  it should "solve part 2 test" in :
    solvePart2(Seq("mjqjpqmgbljsphdztnvjfqwrcgsmlb")) shouldBe 19
    solvePart2(Seq("bvwbjplbgvbhsrlpgdmjqwftvncz")) shouldBe 23
    solvePart2(Seq("nppdvjthqldpwncqszvftbrmjlhg")) shouldBe 23
    solvePart2(Seq("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")) shouldBe 29
    solvePart2(Seq("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")) shouldBe 26

  it should "solve part 2" in :
    solvePart2(input) shouldBe 3380
