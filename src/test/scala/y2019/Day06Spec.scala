package y2019

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day06Spec extends AnyFlatSpec with Matchers with Day06 {
  lazy val input: Seq[String] = Loader.is(this, "day06.txt")

  it should "basics" in {
    part1(
      """
        |COM)B
        |B)C
        |C)D
        |D)E
        |E)F
        |B)G
        |G)H
        |D)I
        |E)J
        |J)K
        |K)L
        |""".stripMargin.trim.linesIterator.toSeq) shouldBe 42
  }

  it should "solve part 1" in {
    part1(input) shouldBe 140608
  }

  it should "solve part 2 test" in {
    part2(
      """
        |COM)B
        |B)C
        |C)D
        |D)E
        |E)F
        |B)G
        |G)H
        |D)I
        |E)J
        |J)K
        |K)L
        |K)YOU
        |I)SAN
        |""".stripMargin.trim.linesIterator.toIndexedSeq) shouldBe 4
  }
  it should "solve part 2" in {
    part2(input) shouldBe 337
  }
}
