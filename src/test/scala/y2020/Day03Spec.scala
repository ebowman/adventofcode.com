package y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day03Spec extends AnyFlatSpec with Matchers {

  import y2020.Day03._

  private val testInput =
    """
      |..##.......
      |#...#...#..
      |.#....#..#.
      |..#.#...#.#
      |.#...##..#.
      |..#.##.....
      |.#.#.#....#
      |.#........#
      |#.##...#...
      |#...##....#
      |.#..#...#.#""".stripMargin.trim.split("\n").toIndexedSeq
  private val liveInput = util.Loader(this, "day03.txt").toIndexedSeq
  private val slopes = Seq(defaultSlope, (1, 1), (5, 1), (7, 1), (1, 2))
  private val testBoards = slopes.map(slope => Board(testInput, slope))
  private val liveBoards = slopes.map(slope => Board(liveInput, slope))

  private def product(a: Long, b: Int): Long = a * b

  "Driver" should "do pass the given test case" in {
    Board(testInput).solve shouldBe 7
  }

  it should "solve the puzzle (part 1)" in {
    Board(liveInput).solve shouldBe 176
  }

  it should "solve the second puzzle test case" in {
    testBoards.map(_.solve).foldLeft(1L)(product) shouldBe 336
  }

  it should "solve the second puzzle live case" in {
    liveBoards.map(_.solve).foldLeft(1L)(product) shouldBe 5872458240L
  }
}
