package y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day16Spec extends AnyFlatSpec with Matchers with Day16 {

  lazy val input: IndexedSeq[String] = Loader(this, "day16.txt").toIndexedSeq

  lazy val testInput =
    """
      |class: 1-3 or 5-7
      |row: 6-11 or 33-44
      |seat: 13-40 or 45-50
      |
      |your ticket:
      |7,1,14
      |
      |nearby tickets:
      |7,3,47
      |40,4,50
      |55,2,20
      |38,6,12""".stripMargin.trim.linesIterator.toIndexedSeq

  it should "solve part 1 test" in {
    part1(testInput) shouldBe 71
  }

  it should "solve part 1" in {
    part1(Loader(this, "day16.txt").toIndexedSeq) shouldBe 19087
  }

  it should "solve part 2 test" in {
    part2(
      """
        |class: 0-1 or 4-19
        |row: 0-5 or 8-19
        |seat: 0-13 or 16-19
        |
        |your ticket:
        |11,12,13
        |
        |nearby tickets:
        |3,9,18
        |15,1,5
        |5,14,9""".stripMargin.trim.linesIterator.toIndexedSeq) shouldBe 1
  }

  it should "solve part 2" in {
    part2(Loader(this, "day16.txt").toIndexedSeq) shouldBe 1382443095281L
  }
}
