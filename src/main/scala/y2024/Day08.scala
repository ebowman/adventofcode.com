package y2024

import scala.annotation.tailrec

class Day08 extends util.Day(8):

  case class Pt(row: Int, col: Int)

  case class Vec(dr: Int, dc: Int)

  private def parseInput(input: IndexedSeq[String]) =
    val height = input.length
    val width = input.head.length
    val acc = Map.empty[Char, List[(Int, Int)]].withDefault(_ => Nil)
    val antennaGroups = input.zipWithIndex.foldLeft(acc):
      case (acc, (line, row)) =>
        line.zipWithIndex.foldLeft(acc):
          case (map, (char, col)) if char != '.' =>
            map.updated(char, (row, col) :: map(char))
          case (map, _) => map
    (height, width, antennaGroups)
  end parseInput

  def solvePart1(input: IndexedSeq[String]): Int =
    val (height, width, antennaGroups) = parseInput(input)

    val antinodes = for
      positions <- antennaGroups.values
      (p1, i) <- positions.zipWithIndex
      p2 <- positions.drop(i + 1)
      antinode <- calculateDoubleDistanceAntinodes(Pt(p1._1, p1._2), Pt(p2._1, p2._2))
      if isInBounds(antinode, height, width)
    yield antinode

    antinodes.toSet.size
  end solvePart1

  private def calculateDoubleDistanceAntinodes(p1: Pt, p2: Pt): Set[Pt] =
    val diff = Vec(p2.row - p1.row, p2.col - p1.col)
    Set(
      Pt(p1.row - diff.dr, p1.col - diff.dc),
      Pt(p2.row + diff.dr, p2.col + diff.dc)
    )

  private def isInBounds(p: Pt, height: Int, width: Int): Boolean =
    p.row >= 0 && p.row < height && p.col >= 0 && p.col < width

  def solvePart2(input: IndexedSeq[String]): Int =
    val (height, width, antennaGroups) = parseInput(input)

    val allAntinodes = for
      positions <- antennaGroups.values
      if positions.size >= 2
      points = positions.map(p => Pt(p._1, p._2))
      lines = findAllLines(points)
      point <- generateAntinodesForLine(lines, height, width)
    yield point

    allAntinodes.toSet.size
  end solvePart2

  private def findAllLines(points: List[Pt]): Set[(Pt, Vec)] =
    val pairs = for
      (p1, i) <- points.zipWithIndex
      p2 <- points.drop(i + 1)
    yield (p1, p2)

    pairs.map:
      case (p1, p2) =>
        val dr = p2.row - p1.row
        val dc = p2.col - p1.col
        val gcdValue = gcd(dr, dc)
        val direction = Vec(dr / gcdValue, dc / gcdValue)
        (p1, direction)
    .toSet
  end findAllLines

  private def generateAntinodesForLine(lines: Set[(Pt, Vec)], height: Int, width: Int): Set[Pt] =
    lines.flatMap:
      case (start, dir) =>
        for
          t <- -math.max(height, width) to math.max(height, width)
          point = Pt(start.row + t * dir.dr, start.col + t * dir.dc)
          if isInBounds(point, height, width)
        yield point
  end generateAntinodesForLine

  @tailrec private def gcd(a: Int, b: Int): Int =
    if b == 0 then math.abs(a) else gcd(b, a % b)
