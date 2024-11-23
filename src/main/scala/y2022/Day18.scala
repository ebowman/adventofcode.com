package y2022

import scala.annotation.tailrec

trait Day18:

  case class Point(x: Int, y: Int, z: Int):
    def neighbors: Set[Point] =
      Set(
        Point(x + 1, y, z), Point(x - 1, y, z),
        Point(x, y + 1, z), Point(x, y - 1, z),
        Point(x, y, z + 1), Point(x, y, z - 1)
      )

  def parseLine(line: String): Point =
    line.split(",").map(_.toInt) match
      case Array(x, y, z) => Point(x, y, z)
      case _ => throw IllegalArgumentException(s"Invalid input: $line")

  def countExposedSides(points: Set[Point]): Int =
    points.toSeq.map: point =>
      6 - point.neighbors.count(points.contains)
    .sum

  def getBounds(points: Set[Point]): (Point, Point) =
    val minX = points.map(_.x).min - 1
    val maxX = points.map(_.x).max + 1
    val minY = points.map(_.y).min - 1
    val maxY = points.map(_.y).max + 1
    val minZ = points.map(_.z).min - 1
    val maxZ = points.map(_.z).max + 1
    (Point(minX, minY, minZ), Point(maxX, maxY, maxZ))

  def isInBounds(point: Point, min: Point, max: Point): Boolean =
    point.x >= min.x && point.x <= max.x &&
      point.y >= min.y && point.y <= max.y &&
      point.z >= min.z && point.z <= max.z

  def floodFill(cubes: Set[Point]): Int =
    val (min, max) = getBounds(cubes)

    @tailrec
    def expand(current: Set[Point], visited: Set[Point]): Set[Point] =
      if current.isEmpty then visited
      else
        val nextPoints = for
          point <- current
          neighbor <- point.neighbors
          if !visited.contains(neighbor)
          if !cubes.contains(neighbor)
          if isInBounds(neighbor, min, max)
        yield neighbor

        expand(nextPoints, visited ++ current)
    end expand

    val exterior = expand(Set(min), Set.empty)

    cubes.toSeq.map: cube =>
      cube.neighbors.count(exterior.contains)
    .sum
  end floodFill

  def solvePart1(input: Seq[String]): Int =
    val points = input.map(parseLine).toSet
    countExposedSides(points)

  def solvePart2(input: Seq[String]): Int =
    val points = input.map(parseLine).toSet
    floodFill(points)

end Day18
