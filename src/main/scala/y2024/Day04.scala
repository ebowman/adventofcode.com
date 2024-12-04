package y2024

import scala.util.Try

class Day04 extends util.Day(4):
  private val directions = for
    x <- -1 to 1
    y <- -1 to 1
    if (x,y) != (0,0)
  yield (x, y)

  def solvePart1(input: IndexedSeq[String]): Int =
    def checkXmas(row: Int, col: Int, dRow: Int, dCol: Int): Boolean =
      "XMAS".indices.forall: i =>
        Try:
          val (r, c) = (row + dRow * i, col + dCol * i)
          input(r)(c) == "XMAS"(i)
        .getOrElse(false)

    (for
      row <- input.indices
      col <- input(0).indices
    yield directions.count(d => checkXmas(row, col, d.head, d.last))).sum

  def solvePart2(input: IndexedSeq[String]): Int =
    def checkDiagonals(row: Int, col: Int): Boolean =
        Try:
          val diag1 = s"${input(row-1)(col-1)}${input(row+1)(col+1)}"
          val diag2 = s"${input(row-1)(col+1)}${input(row+1)(col-1)}"
          Set("MS", "SM").contains(diag1) && Set("MS", "SM").contains(diag2)
        .getOrElse(false)

    (for
      row <- input.indices
      col <- input(0).indices
    yield (row, col)).count: (row, col) =>
      input(row)(col) == 'A' && checkDiagonals(row, col)
