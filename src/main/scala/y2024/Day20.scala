package y2024

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.Try

class Day20 extends util.Day(20):

  private val directions = List((0, 1), (1, 0), (0, -1), (-1, 0))

  def solvePart1(input: IndexedSeq[String]): Any =
    solve(input, maxDist = 2)

  def solvePart2(input: IndexedSeq[String]): Any =
    solve(input, maxDist = 20)

  def solve(input: IndexedSeq[String], maxDist: Int): Int =
    val grid = input.map(_.toArray).toArray
    val (endX, endY) = findEnd(grid)
    val updatedGrid = grid.updated(endY, grid(endY).updated(endX, '.'))
    val distances = bfs(updatedGrid, endX, endY)
    countCheats(distances, maxDist)
  end solve

  private def findEnd(grid: Array[Array[Char]]): (Int, Int) =
    val width = grid.head.length

    @tailrec
    def recurse(x: Int, y: Int): (Int, Int) =
      if (x >= width) recurse(0, y + 1)
      else if (grid(y)(x) == 'E') (x, y)
      else recurse(x + 1, y)

    recurse(0, 0)
  end findEnd

  private def bfs(grid: Array[Array[Char]], startX: Int, startY: Int): Map[(Int, Int), Int] =

    def neighbors(x: Int, y: Int): List[(Int, Int)] =
      directions.map: (dx, dy) =>
        (x + dx, y + dy)
      .filter: (nx, ny) =>
        Try:
          grid(ny)(nx) != '#'
        .getOrElse(false)
    end neighbors

    @tailrec
    def recurse(q: Queue[((Int, Int), Int)], visited: Map[(Int, Int), Int]): Map[(Int, Int), Int] =
      q.dequeueOption match
        case None => visited
        case Some(((pos, dist), restQ)) =>
          val next = neighbors(pos._1, pos._2).filterNot(visited.contains)
          val newVisited = visited ++ next.map(_ -> (dist + 1))
          val newQ = restQ.enqueueAll(next.map(_ -> (dist + 1)))
          recurse(newQ, newVisited)
    end recurse

    val start = (startX, startY)
    recurse(Queue((start, 0)), Map(start -> 0))
  end bfs

  private def countCheats(distances: Map[(Int, Int), Int], maxDist: Int): Int =
    val keys = distances.keys.toIndexedSeq
    val size = keys.size

    (0 until size).foldLeft(0):
      case (acc, i) =>
        (0 until size).foldLeft(acc):
          case (innerAcc, j) if i == j => innerAcc
          case (innerAcc, j) =>
            val (w1, w2) = (keys(i), keys(j))
            val d = manhattanDist(w1, w2)
            if d <= maxDist && (distances(w1) - distances(w2) - d >= 100) then
              innerAcc + 1
            else
              innerAcc
  end countCheats

  private inline def manhattanDist(a: (Int, Int), b: (Int, Int)): Int =
    (a._1 - b._1).abs + (a._2 - b._2).abs

end Day20
