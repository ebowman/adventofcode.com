package y2022

import scala.annotation.tailrec

trait Day14:
  case class Point(x: Int, y: Int)
  case class Cave(rocks: Set[Point], sand: Set[Point], maxY: Int, hasFloor: Boolean = false):
    def isFree(p: Point): Boolean =
      if hasFloor && p.y >= maxY + 2 then false
      else !rocks.contains(p) && !sand.contains(p)
    def addSand(p: Point): Cave = copy(sand = sand + p)
    def isSourceBlocked: Boolean = sand.contains(Point(500, 0))

  private def parseInput(input: Seq[String]): Set[Point] =
    val rockPaths = input.map: line =>
      line.split(" -> ")
        .map: coord =>
          val Array(x, y) = coord.split(",").map(_.toInt)
          Point(x, y)
        .toSeq

    rockPaths.flatMap: path =>
      path.zip(path.tail).flatMap: (start, end) =>
        if start.x == end.x then
          val (minY, maxY) = (start.y.min(end.y), start.y.max(end.y))
          (minY to maxY).map(y => Point(start.x, y))
        else
          val (minX, maxX) = (start.x.min(end.x), start.x.max(end.x))
          (minX to maxX).map(x => Point(x, start.y))
    .toSet

  private def nextSandPosition(p: Point, cave: Cave): Option[Point] =
    val moves = List(
      Point(p.x, p.y + 1),      // down
      Point(p.x - 1, p.y + 1),  // down-left
      Point(p.x + 1, p.y + 1)   // down-right
    )
    moves.find(cave.isFree)

  @tailrec
  private def simulateSand(current: Point, cave: Cave): Option[Cave] =
    if !cave.hasFloor && current.y >= cave.maxY then None
    else
      nextSandPosition(current, cave) match
        case None => Some(cave.addSand(current))
        case Some(next) => simulateSand(next, cave)

  @tailrec
  private def pourSand(cave: Cave, count: Int = 0): Int =
    if cave.isSourceBlocked then count
    else
      simulateSand(Point(500, 0), cave) match
        case None => count
        case Some(newCave) => pourSand(newCave, count + 1)

  def solve(input: Seq[String], part2: Boolean): Int =
    val rocks = parseInput(input)
    val maxY = rocks.map(_.y).max
    val cave = Cave(rocks, Set.empty, maxY, hasFloor = part2)
    pourSand(cave)
    
end Day14
