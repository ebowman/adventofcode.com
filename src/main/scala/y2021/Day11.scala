package y2021

import scala.util.Try

trait Day11 {
  def iter(array: Array[Array[Int]]): Int = {
    var work: List[(Int, Int)] = Nil
    var flashed: Set[(Int, Int)] = Set()
    for (y <- array.indices; x <- array(y).indices) {
      array(y)(x) += 1
      if (array(y)(x) > 9) {
        work ::= (y, x)
        flashed += ((y, x))
      }
    }
    var count = 0
    while (work.nonEmpty) {
      val (y, x) = work.head
      work = work.tail
      count += 1
      for (i <- -1 to 1; j <- -1 to 1 if i != 0 || j != 0) {
        val (yp, xp) = (y + i, x + j)
        Try {
          array(yp)(xp) += 1
          if (array(yp)(xp) > 9 && !flashed.contains((yp, xp))) {
            work ::= (yp, xp)
            flashed += ((yp, xp))
          }
        }
      }
      flashed.foreach { case (y, x) => array(y)(x) = 0 }
    }
    count
  }

  def solve1(input: Seq[String]): Long = {
    val array = input.map(_.map(_ - '0').toArray).toArray
    (for (_ <- 1 to 100) yield iter(array)).sum
  }

  def solve2(input: Seq[String]): Int = {
    val array = input.map(_.map(_ - '0').toArray).toArray
    var allZeroes = false
    var count = 0
    while (!allZeroes) {
      iter(array)
      allZeroes = array.forall(row => row.forall(_ == 0))
      count += 1
    }
    count
  }
}