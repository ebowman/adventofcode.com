package y2016

import scala.annotation.tailrec

trait Day15 {
  def solve1(input: Seq[String]): Int = {
    val R = """.*?(\d+) positions;.*?position (\d+)\.""".r
    val board = input.map { case R(positions, position) => Seq(position, positions).map(_.toInt) }

    def pass(t: Int): Boolean =
      board.foldLeft((t, true)) {
        case ((t, true), Seq(init, max)) => (t + 1, ((init + t + 1) % max) == 0)
        case ((t, false), _) => (t, false)
      }._2

    @tailrec def recurse(t: Int): Int =
      if (pass(t)) t
      else recurse(t + 1)

    recurse(0)
  }
}
