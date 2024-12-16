package y2024

import scala.annotation.tailrec
import scala.util.Try

case class Day16() extends util.Day(16):
  def solvePart1(input: IndexedSeq[String]): Int =
    PathFinder(Grid(input)).findMinimumCost

  def solvePart2(input: IndexedSeq[String]): Int =
    PathFinder(Grid(input)).countPositionsOnOptimalPath

  enum Direction:
    case North, East, South, West

    def opposite: Direction = this match
      case North => South
      case East => West
      case South => North
      case West => East

    def delta: (Int, Int) = this match
      case North => (0, -1)
      case East => (1, 0)
      case South => (0, 1)
      case West => (-1, 0)
  end Direction

  case class Position(x: Int, y: Int)

  case class State(pos: Position, direction: Direction)

  case class Grid(cells: IndexedSeq[String]):
    val height: Int = cells.size
    val width: Int = cells.head.length

    def isWall(pos: Position): Boolean =
      pos.x < 0 || pos.x >= width ||
        pos.y < 0 || pos.y >= height ||
        cells(pos.y)(pos.x) == '#'

    def findPosition(target: Char): Position =
      val y = cells.indexWhere(_.contains(target))
      val x = cells(y).indexWhere(_ == target)
      Position(x, y)
  end Grid

  private case class PathFinder(grid: Grid):
    private val startPos = grid.findPosition('S')
    private val endPos = grid.findPosition('E')

    def findMinimumCost: Int =
      val paths = findShortestPaths(startPos, reversed = false)
      Direction.values
        .flatMap(d => paths.get((endPos, d)))
        .min

    private def findShortestPaths(start: Position,
                                  reversed: Boolean): Map[(Position, Direction), Int] =
      val initialStates = if !reversed then
        List((0, State(start, Direction.East)))
      else
        Direction.values.map(d => (0, State(start, d))).toList

      def nextStates(state: State, cost: Int): List[(Int, State)] =
        if !reversed then forwardMoves(state, cost)
        else backwardMoves(state, cost)

      def forwardMoves(state: State, cost: Int): List[(Int, State)] =
        val delta = state.direction.delta
        val newPos = Position(
          state.pos.x + delta._1,
          state.pos.y + delta._2
        )
        val forward = Option.when(!grid.isWall(newPos))(
          (cost + 1, State(newPos, state.direction))
        )
        val turns = List(-1, 1).map: turn =>
          val newIndex = (4 + Direction.values.indexOf(state.direction) + turn) % 4
          (cost + 1000, State(state.pos, Direction.values(newIndex)))
        forward.toList ++ turns
      end forwardMoves

      def backwardMoves(state: State, cost: Int): List[(Int, State)] =
        val delta = state.direction.delta
        val newPos = Position(
          state.pos.x - delta._1,
          state.pos.y - delta._2
        )
        val backward = Option.when(!grid.isWall(newPos))(
          (cost + 1, State(newPos, state.direction))
        )
        val turns = Direction.values
          .filterNot(_ == state.direction)
          .map(d => (cost + 1000, State(state.pos, d)))
        backward.toList ++ turns
      end backwardMoves

      @tailrec
      def dijkstra(queue: List[(Int, State)],
                   distances: Map[(Position, Direction), Int]): Map[(Position, Direction), Int] =
        queue match
          case Nil => distances
          case (cost, state) :: rest =>
            val key = (state.pos, state.direction)
            if distances.get(key).exists(_ < cost) then
              dijkstra(rest, distances)
            else
              val candidates = nextStates(state, cost)
              val (newDistances, newQueue) = candidates.foldLeft((distances, rest)):
                case ((dists, q), (nextCost, nextState)) =>
                  val nextKey = (nextState.pos, nextState.direction)
                  if dists.get(nextKey).exists(_ <= nextCost) then (dists, q)
                  else (dists.updated(nextKey, nextCost), (nextCost, nextState) :: q)
              dijkstra(
                newQueue.sortBy(_._1),
                newDistances
              )
      end dijkstra

      dijkstra(
        initialStates,
        initialStates.map((c, s) => ((s.pos, s.direction), c)).toMap
      )
    end findShortestPaths

    def countPositionsOnOptimalPath: Int =
      val forwardPaths = findShortestPaths(startPos, reversed = false)
      val minCost = Direction.values
        .flatMap(d => forwardPaths.get((endPos, d)))
        .min
      val backwardPaths = findShortestPaths(endPos, reversed = true)

      (for
        y <- 0 until grid.height
        x <- 0 until grid.width
        pos = Position(x, y)
        if !grid.isWall(pos)
        if Direction.values.exists: dir =>
          val key = (pos, dir)
          Try:
            forwardPaths(key) + backwardPaths(key) == minCost
          .getOrElse(false)
      yield 1).size
    end countPositionsOnOptimalPath
  end PathFinder
end Day16
