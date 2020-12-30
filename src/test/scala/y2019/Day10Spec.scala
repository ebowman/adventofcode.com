package y2019

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day10Spec extends AnyFlatSpec with Matchers with Day10 {

  lazy val input: IndexedSeq[String] = Loader.is(this, "day10.txt")

  it should "solve part 1 tests" in {
    part1(
      """
        |.#..#
        |.....
        |#####
        |....#
        |...##""".stripMargin.trim.linesIterator.toIndexedSeq) shouldBe 8

    part1(
      """
        |......#.#.
        |#..#.#....
        |..#######.
        |.#.#.###..
        |.#..#.....
        |..#....#.#
        |#..#....#.
        |.##.#..###
        |##...#..#.
        |.#....####""".stripMargin.trim.linesIterator.toIndexedSeq) shouldBe 33

    part1(
      """
        |#.#...#.#.
        |.###....#.
        |.#....#...
        |##.#.#.#.#
        |....#.#.#.
        |.##..###.#
        |..#...##..
        |..##....##
        |......#...
        |.####.###.""".stripMargin.trim.linesIterator.toIndexedSeq) shouldBe 35

    part1(
      """
        |.#..#..###
        |####.###.#
        |....###.#.
        |..###.##.#
        |##.##.#.#.
        |....###..#
        |..#.#..#.#
        |#..#.#.###
        |.##...##.#
        |.....#.#..
        |""".stripMargin.trim.linesIterator.toIndexedSeq) shouldBe 41

    part1(
      """
        |.#..##.###...#######
        |##.############..##.
        |.#.######.########.#
        |.###.#######.####.#.
        |#####.##.#.##.###.##
        |..#####..#.#########
        |####################
        |#.####....###.#.#.##
        |##.#################
        |#####.##.###..####..
        |..######..##.#######
        |####.##.####...##..#
        |.#####..#.######.###
        |##...#.##########...
        |#.##########.#######
        |.####.#.###.###.#.##
        |....##.##.###..#####
        |.#.#.###########.###
        |#.#.#.#####.####.###
        |###.##.####.##.#..##""".stripMargin.trim.linesIterator.toIndexedSeq) shouldBe 210
  }

  it should "solve part 1" in {
    part1(input) shouldBe 221
  }

  it should "solve part 2 tests" in {
    val result = part2(
      """
        |.#..##.###...#######
        |##.############..##.
        |.#.######.########.#
        |.###.#######.####.#.
        |#####.##.#.##.###.##
        |..#####..#.#########
        |####################
        |#.####....###.#.#.##
        |##.#################
        |#####.##.###..####..
        |..######..##.#######
        |####.##.####...##..#
        |.#####..#.######.###
        |##...#.##########...
        |#.##########.#######
        |.####.#.###.###.#.##
        |....##.##.###..#####
        |.#.#.###########.###
        |#.#.#.#####.####.###
        |###.##.####.##.#..##""".stripMargin.trim.linesIterator.toIndexedSeq)
    result(199) shouldBe(8, 2)
  }

  it should "solve part 2" in {
    val result = part2(input)(199)
    result._1 * 100 + result._2 shouldBe 806
  }
}
