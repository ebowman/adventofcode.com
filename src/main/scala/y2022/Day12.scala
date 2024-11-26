package y2022

import scala.annotation.tailrec
import scala.collection.immutable.Queue

trait Day12:
  case class Point(x: Int, y: Int)
  case class Grid(heights: Seq[Seq[Int]], start: Point, end: Point)

  def parseInput(input: Seq[String]): Grid =
    var start = Point(0, 0)
    var end = Point(0, 0)

    val heights = input.zipWithIndex.map: (line, y) =>
      line.zipWithIndex.map: (char, x) =>
        char match
          case 'S' =>
            start = Point(x, y)
            0 // 'a' elevation
          case 'E' =>
            end = Point(x, y)
            25 // 'z' elevation
          case c => c - 'a'

    Grid(heights, start, end)

  private def getNeighbors(current: Point, grid: Seq[Seq[Int]]): List[Point] =
    val directions = List(
      Point(0, 1), Point(0, -1), Point(1, 0), Point(-1, 0)
    )

    directions.map(d => Point(current.x + d.x, current.y + d.y))
      .filter: p =>
        p.x >= 0 && p.x < grid.head.length &&
          p.y >= 0 && p.y < grid.length &&
          grid(p.y)(p.x) <= grid(current.y)(current.x) + 1

  private def findShortestPath(grid: Grid, start: Point, isEnd: Point => Boolean): Int =
    val distances = scala.collection.mutable.Map[Point, Int]()
    val queue = Queue((start, 0))
    distances(start) = 0

    @tailrec
    def bfs(queue: Queue[(Point, Int)]): Int =
      if queue.isEmpty then Int.MaxValue
      else
        val ((current, steps), newQueue) = queue.dequeue
        if isEnd(current) then steps
        else
          val unvisitedNeighbors = getNeighbors(current, grid.heights)
            .filterNot(distances.contains)

          val nextQueue = unvisitedNeighbors.foldLeft(newQueue): (q, neighbor) =>
            distances(neighbor) = steps + 1
            q.enqueue((neighbor, steps + 1))

          bfs(nextQueue)
    end bfs

    bfs(queue)
  end findShortestPath

  def solvePart1(input: Seq[String]): Int =
    val grid = parseInput(input)
    findShortestPath(grid, grid.start, _ == grid.end)

  def solvePart2(input: Seq[String]): Int =
    val grid = parseInput(input)
    val startPoints = for
      y <- grid.heights.indices
      x <- grid.heights.head.indices
      if grid.heights(y)(x) == 0
    yield Point(x, y)

    startPoints.map(start =>
      findShortestPath(grid, start, _ == grid.end)
    ).min

end Day12
