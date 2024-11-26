package y2022

trait Day15:
  case class Point(x: Int, y: Int):
    def manhattanDistance(other: Point): Int =
      (x - other.x).abs + (y - other.y).abs

  case class Sensor(position: Point, closestBeacon: Point):
    lazy val range: Int = position.manhattanDistance(closestBeacon)

    def getCoverageAtY(y: Int): Option[Range] =
      val yDist = (position.y - y).abs
      if yDist > range then None
      else
        val xSpread = range - yDist
        Some(Range(position.x - xSpread, position.x + xSpread))

  case class Range(start: Int, end: Int):
    def clamp(min: Int, max: Int): Range =
      Range(start.max(min), end.min(max))

    def isEmpty: Boolean = end < start

  def parseInput(input: Seq[String]): Seq[Sensor] =
    val pattern = """Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)""".r
    input.map:
      case pattern(sx, sy, bx, by) =>
        Sensor(
          Point(sx.toInt, sy.toInt),
          Point(bx.toInt, by.toInt)
        )

  def mergeRanges(ranges: Seq[Range]): Seq[Range] =
    if ranges.isEmpty then Seq.empty
    else
      val sorted = ranges.sortBy(_.start)
      sorted.tail.foldLeft(Seq(sorted.head)): (acc, range) =>
        val last = acc.last
        if range.start <= last.end + 1 then
          acc.init :+ Range(last.start, range.end.max(last.end))
        else
          acc :+ range
  end mergeRanges

  def findGapInCoverage(ranges: Seq[Range], min: Int, max: Int): Option[Int] =
    val clampedRanges = ranges
      .map(_.clamp(min, max))
      .filterNot(_.isEmpty)

    val merged = mergeRanges(clampedRanges)
    if merged.length > 1 then Some(merged.head.end + 1)
    else if merged.head.start > min then Some(min)
    else if merged.head.end < max then Some(max)
    else None
  end findGapInCoverage

  def solvePart1(input: Seq[String], targetY: Int = 2000000): Int =
    val sensors = parseInput(input)
    val beaconsAtY = sensors
      .map(_.closestBeacon)
      .filter(_.y == targetY)
      .map(_.x)
      .toSet

    val coverageRanges = sensors
      .flatMap(_.getCoverageAtY(targetY))

    val mergedRanges = mergeRanges(coverageRanges)

    val coveredPositions = mergedRanges
      .map(r => r.end - r.start + 1)
      .sum

    coveredPositions - beaconsAtY.size
  end solvePart1

  def solvePart2(input: Seq[String], searchSpace: Int = 4000000): Long =
    val sensors = parseInput(input)

    val result = (0 to searchSpace).view
      .map: y =>
        val ranges = sensors.flatMap(_.getCoverageAtY(y))
        findGapInCoverage(ranges, 0, searchSpace).map((_, y))
      .find(_.isDefined)
      .flatten

    result match
      case Some((x, y)) => x.toLong * 4000000L + y
      case None => throw new RuntimeException("No solution found")
end Day15
