/*
See https://adventofcode.com/2015/day/25
 */

package y2015

/* Advances (1,1) -> (2, 1) -> (1, 2) -> (3, 1) -> (2, 2) -> (1, 3) */
case class Cursor(row: Int, column: Int) {
  def next: Cursor = if (row - 1 == 0) Cursor(row + column, 1) else Cursor(row - 1, column + 1)
}

object Cursor {
  // top left of the infinite page
  lazy val origin = Cursor(1, 1)
}

case class Generator(cursor: Cursor, value: BigInt) {
  def next: Generator = Generator(cursor.next, (value * 252533) % 33554393)
}

object Generator {
  lazy val origin = Generator(Cursor.origin, 20151125)
}

