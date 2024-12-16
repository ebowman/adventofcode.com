package y2024

import scala.annotation.{tailrec, targetName}
import scala.util.Try

case class Day16() extends util.Day(16):
  def solvePart1(input: IndexedSeq[String]): Int =
    PathFinder(Grid(input)).findMinimumCost

  def solvePart2(input: IndexedSeq[String]): Int =
    PathFinder(Grid(input)).countPositionsOnOptimalPath

  case class Vec2(x: Int, y: Int):
    @targetName("add")
    def +(other: Vec2): Vec2 = Vec2(x + other.x, y + other.y)

    @targetName("subtract")
    def -(other: Vec2): Vec2 = Vec2(x - other.x, y - other.y)
  end Vec2

  case class State(pos: Vec2, direction: Vec2)

  case class Grid(cells: IndexedSeq[String]):
    val height: Int = cells.size
    val width: Int = cells.head.length

    def isWall(pos: Vec2): Boolean =
      pos.x < 0 || pos.x >= width ||
        pos.y < 0 || pos.y >= height ||
        cells(pos.y)(pos.x) == '#'

    def findPosition(target: Char): Vec2 =
      val y = cells.indexWhere(_.contains(target))
      val x = cells(y).indexWhere(_ == target)
      Vec2(x, y)
  end Grid

  private case class PathFinder(grid: Grid):
    private val startPos = grid.findPosition('S')
    private val endPos = grid.findPosition('E')

    def findMinimumCost: Int =
      val paths = findShortestPaths(startPos, reversed = false)
      Directions
        .flatMap(d => paths.get((endPos, d)))
        .min

    def countPositionsOnOptimalPath: Int =
      val forwardPaths = findShortestPaths(startPos, reversed = false)
      val backwardPaths = findShortestPaths(endPos, reversed = true)
      val minCost = Directions
        .flatMap(d => forwardPaths.get((endPos, d)))
        .min

      (0 until grid.height).flatMap: y =>
        (0 until grid.width).map: x =>
          val pos = Vec2(x, y)
          !grid.isWall(pos) && Directions.exists: dir =>
            Try:
              val key = (pos, dir)
              forwardPaths(key) + backwardPaths(key) == minCost
            .getOrElse(false)
      .count(identity)

    end countPositionsOnOptimalPath

    private def findShortestPaths(start: Vec2, reversed: Boolean): Map[(Vec2, Vec2), Int] =
      val initialStates =
        if !reversed then List((0, State(start, Directions.East)))
        else Directions.map(d => (0, State(start, d))).toList

      @tailrec
      def dijkstra(queue: List[(Int, State)],
                   distances: Map[(Vec2, Vec2), Int]): Map[(Vec2, Vec2), Int] =
        queue match
          case Nil => distances
          case (cost, state) :: rest =>
            val key = (state.pos, state.direction)
            if distances.get(key).exists(_ < cost) then
              dijkstra(rest, distances)
            else
              val candidates = neighbors(state, cost, reversed)
              val (newDistances, newQueue) = candidates.foldLeft((distances, rest)):
                case ((dists, q), (nextCost, nextState)) =>
                  val nextKey = (nextState.pos, nextState.direction)
                  if dists.get(nextKey).exists(_ <= nextCost) then (dists, q)
                  else (dists.updated(nextKey, nextCost), (nextCost, nextState) :: q)
              dijkstra(newQueue.sortBy(_._1), newDistances)
      end dijkstra

      dijkstra(initialStates, initialStates.map((c, s) => ((s.pos, s.direction), c)).toMap)
    end findShortestPaths

    private def neighbors(state: State, cost: Int, reversed: Boolean): List[(Int, State)] =
      val delta = state.direction

      val forwardPos = if reversed then state.pos - delta else state.pos + delta
      val forward = Option.when(!grid.isWall(forwardPos)):
        (cost + 1, State(forwardPos, state.direction))

      val currentIndex = Directions.indexOf(state.direction)
      val turns = List(-1, 1).map: off =>
        val newDir = Directions((currentIndex + off + Directions.size) % Directions.size)
        (cost + 1000, state.copy(direction = newDir))

      forward.map(_ :: turns).getOrElse(turns)

    end neighbors
  end PathFinder

  object Directions extends Seq[Vec2]:

    val North: Vec2 = Vec2(0, -1)
    val East: Vec2 = Vec2(1, 0)
    val South: Vec2 = Vec2(0, 1)
    val West: Vec2 = Vec2(-1, 0)

    private val all: Seq[Vec2] = Vector(North, East, South, West)

    override def apply(idx: Int): Vec2 = all(idx)
    override def length: Int = all.length
    override def iterator: Iterator[Vec2] = all.iterator

  end Directions
end Day16
