package y2019

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day03Spec extends AnyFlatSpec with Matchers with Day03 {

  lazy val input: IndexedSeq[String] = Loader.is(this, "day03.txt")

  it should "solve part 1 test" in {
    part1(
      """
        |
        |R75,D30,R83,U83,L12,D49,R71,U7,L72
        |U62,R66,U55,R34,D71,R55,D58,R83
        |
        |""".stripMargin.trim.linesIterator.toIndexedSeq) shouldBe 159
    part1(
      """
        |
        |R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
        |U98,R91,D20,R16,D67,R40,U7,R15,U6,R7
        |
        |""".stripMargin.trim.linesIterator.toIndexedSeq) shouldBe 135
  }

  it should "solve part 1" in {
    part1(input) shouldBe 207
  }

  it should "solve part 2 test" in {
    part2(
      """
        |
        |R8,U5,L5,D3
        |U7,R6,D4,L4
        |
        |""".stripMargin.trim.linesIterator.toIndexedSeq) shouldBe 30

    part2(
      """
        |
        |R75,D30,R83,U83,L12,D49,R71,U7,L72
        |U62,R66,U55,R34,D71,R55,D58,R83
        |
        |""".stripMargin.trim.linesIterator.toIndexedSeq) shouldBe 610
    part2(
      """
        |
        |R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
        |U98,R91,D20,R16,D67,R40,U7,R15,U6,R7
        |
        |""".stripMargin.trim.linesIterator.toIndexedSeq) shouldBe 410
  }

  it should "solve part 2" in {
    part2(input) shouldBe 21196
  }
}
