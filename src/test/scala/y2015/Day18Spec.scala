package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day18Spec extends AnyFlatSpec with Matchers with Day18 {

  "The automaton" should "pass the basic tests" in {
    val input =
      """
        |.#.#.#
        |...##.
        |#....#
        |..#...
        |#.#..#
        |####..""".stripMargin.trim.linesIterator.iterator.to(Iterable)
    val board = parse(input)
    board.Cursor(1, 1).lit shouldBe 1
    board.Cursor(4, 2).lit shouldBe 2
    board.Cursor(1, 2).lit shouldBe 2
    board.Cursor(6, 1).lit shouldBe 1
    board.next.next.next.next.lit shouldBe 4
  }

  it should "solve the first puzzle input" in {
    var board = parse(Loader(this, "day18.txt"))
    for (i <- 1 to 100) {
      board = board.next
    }
    board.lit shouldBe 821
  }

  it should "solve the second puzzle input" in {
    var board = parse(Loader(this, "day18.txt"), pinned = true)
    for (i <- 1 to 100) {
      board = board.next
    }
    board.lit shouldBe 886
  }
}
