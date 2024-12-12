package y2024

import scala.annotation.tailrec

class Day12 extends util.Day(12):
  case class Point(row: Int, col: Int)
  case class Grid(cells: Vector[Vector[Char]]):
    val rows: Int = cells.length
    val cols: Int = cells(0).length

    def apply(p: Point): Char = cells(p.row)(p.col)
    def isInBounds(p: Point): Boolean =
      p.row >= 0 && p.row < rows && p.col >= 0 && p.col < cols

  object Direction:
    val all: List[Point] = List(
      Point(-1, 0),
      Point(1, 0),
      Point(0, -1),
      Point(0, 1)
    )

  def solvePart1(input: IndexedSeq[String]): Int = solve(input)
  def solvePart2(input: IndexedSeq[String]): Int = solve(input, isPart2 = true)

  private def solve(input: IndexedSeq[String], isPart2: Boolean = false): Int =
    val grid = Grid(input.map(_.toVector).toVector)

    def getNeighbors(p: Point): Seq[Point] =
      Direction.all
        .map(d => Point(p.row + d.row, p.col + d.col))
        .filter(grid.isInBounds)

    def findConnectedRegion(start: Point, targetChar: Char, visited: Set[Point]): (Set[Point], Set[Point]) =
      @tailrec
      def explore(toVisit: List[Point], region: Set[Point], visited: Set[Point]): (Set[Point], Set[Point]) =
        toVisit match
          case Nil => (region, visited)
          case current :: rest =>
            val newPoints = getNeighbors(current)
              .filter(p => grid(p) == targetChar && !visited.contains(p))
            explore(
              rest ++ newPoints,
              region ++ newPoints,
              visited ++ newPoints
            )

      explore(List(start), Set(start), visited + start)

    def findAllRegions: List[Set[Point]] =
      @tailrec
      def scan(currentPoint: Point = Point(0, 0),
               visited: Set[Point] = Set.empty,
               regions: List[Set[Point]] = Nil): List[Set[Point]] =
        if currentPoint.row >= grid.rows then regions
        else if currentPoint.col >= grid.cols then
          scan(Point(currentPoint.row + 1, 0), visited, regions)
        else if visited.contains(currentPoint) then
          scan(Point(currentPoint.row, currentPoint.col + 1), visited, regions)
        else
          val char = grid(currentPoint)
          val (region, newVisited) = findConnectedRegion(currentPoint, char, visited)
          scan(Point(currentPoint.row, currentPoint.col + 1), newVisited, region :: regions)

      scan()

    def calculateRegionValue(region: Set[Point], isPart2: Boolean): Int =
      val regionSize = region.size
      if !isPart2 then
        val borderCount = calculateBorderCount(region)
        regionSize * borderCount
      else
        val sideCount = calculateSideCount(region)
        regionSize * sideCount

    def calculateBorderCount(region: Set[Point]): Int =
      region.foldLeft(0): (count, point) =>
        val insideNeighbors = getNeighbors(point).count(region.contains)
        count + (4 - insideNeighbors)

    def calculateSideCount(region: Set[Point]): Int =
      Direction.all.map(dir => countSidesInDirection(region, dir)).sum

    findAllRegions.map(region => calculateRegionValue(region, isPart2)).sum

  private def countSidesInDirection(region: Set[Point], direction: Point): Int =
    val border = findBorderPoints(region, direction)
    val redundantPoints = findRedundantPoints(border, direction)
    border.size - redundantPoints.size

  private def findBorderPoints(region: Set[Point], dir: Point): Set[Point] =
    region.foldLeft(Set.empty[Point]): (borderPoints, point) =>
      val neighbor = Point(point.row + dir.row, point.col + dir.col)
      if !region.contains(neighbor) then borderPoints + neighbor else borderPoints

  private def findRedundantPoints(border: Set[Point], dir: Point): Set[Point] =
    def traverse(start: Point): Set[Point] =
      val next = Point(start.row + dir.col, start.col + dir.row)
      if border.contains(next) then traverse(next) + next else Set.empty

    border.foldLeft(Set.empty[Point]): (redundant, point) =>
      redundant ++ traverse(point)

end Day12
