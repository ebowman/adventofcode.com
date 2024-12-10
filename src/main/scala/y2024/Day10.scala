package y2024

import scala.annotation.tailrec
import scala.util.Try

class Day10 extends util.Day(10):

  def solvePart1(input: IndexedSeq[String]): Int =
    val grid = input.map(_.map(_.asDigit).toVector).toVector
    val reachableNines = buildReachableNinesTable(grid)
    (for
      r <- grid.indices
      c <- grid(r).indices
      if grid(r)(c) == 0
    yield reachableNines(r)(c).size).sum

  def solvePart2(input: IndexedSeq[String]): Int =
    val grid = input.map(_.map(_.asDigit).toVector).toVector
    val trailCounts = buildTrailCountsTable(grid)
    (for
      r <- grid.indices
      c <- grid(r).indices
      if grid(r)(c) == 0
    yield trailCounts(r)(c)).sum

  private def buildTrailCountsTable(grid: Vector[Vector[Int]]): Vector[Vector[Int]] =
    val (rows, cols) = (grid.length, grid(0).length)
    val allCells = for r <- 0 until rows; c <- 0 until cols yield (r, c)
    val reachableNines = buildReachableNinesTable(grid)

    @tailrec def recurse(h: Int, acc: Vector[Vector[Int]]): Vector[Vector[Int]] =
      if h < 0 then acc
      else
        val cellsAtHeight = allCells.filter(rc => grid(rc._1)(rc._2) == h)
        val updatedAcc = cellsAtHeight.foldLeft(acc):
          case (innerAcc, (r, c)) =>
            if h == 9 then innerAcc.updated(r, innerAcc(r).updated(c, 1))
            else
              val neighbors = Seq((r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)).filter:
                case (nr, nc) =>
                  nr >= 0 && nr < rows && nc >= 0 && nc < cols && grid(nr)(nc) == h + 1 && reachableNines(nr)(nc).nonEmpty
              val sumCount = neighbors.foldLeft(0):
                case (acc2, (nr, nc)) =>
                  acc2 + innerAcc(nr)(nc)
              innerAcc.updated(r, innerAcc(r).updated(c, sumCount))
        recurse(h - 1, updatedAcc)

    val initialTrailCounts = Vector.fill(rows, cols)(0)
    recurse(9, initialTrailCounts)

  private def buildReachableNinesTable(grid: Vector[Vector[Int]]): Vector[Vector[Set[(Int, Int)]]] =
    val (rows, cols) = (grid.length, grid(0).length)
    val allCells = for r <- 0 until rows; c <- 0 until cols yield (r, c)

    @tailrec def recurse(h: Int, acc: Vector[Vector[Set[(Int, Int)]]]): Vector[Vector[Set[(Int, Int)]]] =
      if h < 0 then acc
      else
        val cellsAtHeight = allCells.filter:
          case (r, c) =>
            grid(r)(c) == h
        val updatedAcc = cellsAtHeight.foldLeft(acc):
          case (innerAcc, rc@(r, c)) =>
            if h == 9 then innerAcc.updated(r, innerAcc(r).updated(c, Set(rc)))
            else
              val neighbors = Seq((r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)).filter:
                case (nr, nc) =>
                  Try(grid(nr)(nc) == h + 1).getOrElse(false)
              val unionSet = neighbors.foldLeft(Set.empty[(Int, Int)]):
                case (acc2, (nr, nc)) =>
                  acc2.union(innerAcc(nr)(nc))
              innerAcc.updated(r, innerAcc(r).updated(c, unionSet))
        recurse(h - 1, updatedAcc)

    val initialReachableNines = Vector.fill(rows, cols)(Set.empty[(Int, Int)])
    recurse(9, initialReachableNines)
end Day10
