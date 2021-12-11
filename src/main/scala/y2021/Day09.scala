package y2021

import scala.util.Try

trait Day09 {

  def solve1(input: Seq[String]): Int = {
    val array = input.map(_.map(_ - '0'))

    def get(y: Int, x: Int) = Try(array(y)(x)).getOrElse(9)

    (for {y <- array.indices
          x <- array(y).indices
          v = get(y, x) if
            v < get(y, x - 1) &&
              v < get(y, x + 1) &&
              v < get(y - 1, x) &&
              v < get(y + 1, x)} yield v + 1).sum
  }

  def solve2(input: Seq[String]): Int = {
    val array = input.map(_.map(_ - '0'))

    def get(y: Int, x: Int) = Try(array(y)(x)).getOrElse(9)

    def get2(pt: (Int, Int)): Int = get(pt._1, pt._2)

    def breadthFirst(seen: Set[(Int, Int)] = Set.empty)(pt: (Int, Int)): Set[(Int, Int)] = {
      val (y, x) = pt
      val v = get(y, x)
      if (v == 9 || seen.contains(pt)) seen
      else {
        Seq((y, x - 1), (y, x + 1), (y - 1, x), (y + 1, x)).filter(
          pt => get2(pt) >= v).foldLeft(seen + pt)(breadthFirst(_)(_))
      }
    }

    val lowPoints = for {
      y <- array.indices
      x <- array(y).indices
      v = get(y, x) if
        v < get(y, x - 1) && v < get(y, x + 1) && v < get(y - 1, x) && v < get(y + 1, x)} yield (y, x)

    lowPoints.map(breadthFirst()).map(_.size).sorted.reverse.take(3).product
  }
}