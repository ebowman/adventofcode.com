package y2024

import scala.annotation.tailrec

class Day08 extends util.Day(8):

  case class Pt(row: Int, col: Int):
    def isInBounds(height: Int, width: Int): Boolean =
      row >= 0 && row < height && col >= 0 && col < width

  private def parseInput(input: IndexedSeq[String]) =
    val height = input.length
    val width = input.head.length
    val acc = Map.empty[Char, List[Pt]].withDefaultValue(Nil)
    val antennaGroups = input.zipWithIndex.foldLeft(acc):
      case (acc, (line, row)) =>
        line.zipWithIndex.foldLeft(acc):
          case (map, (char, col)) if char != '.' =>
            map.updated(char, Pt(row, col) :: map(char))
          case (map, _) => map
    (height, width, antennaGroups)

  def solvePart1(input: IndexedSeq[String]): Int =
    val (height, width, antennaGroups) = parseInput(input)
    val antinodesBuilder = Set.newBuilder[Pt]

    for
      positions <- antennaGroups.values
      i <- positions.indices
      p1 = positions(i)
      j <- 0 until i
      p2 = positions(j)
      dr = p2.row - p1.row
      dc = p2.col - p1.col
      forward = Pt(p2.row + dr, p2.col + dc)
      reverse = Pt(p1.row - dr, p1.col - dc)
    do
      if forward.isInBounds(height, width) then antinodesBuilder += forward
      if reverse.isInBounds(height, width) then antinodesBuilder += reverse

    val antinodes = antinodesBuilder.result()
    antinodes.size

  def solvePart2(input: IndexedSeq[String]): Int =
    val (height, width, antennaGroups) = parseInput(input)
    val antinodesBuilder = Set.newBuilder[Pt]

    def addLineAntinodes(p1: Pt, p2: Pt): Unit =
      val dr = p2.row - p1.row
      val dc = p2.col - p1.col

      @tailrec
      def recurse(pt: Pt): Unit =
        if pt.isInBounds(height, width) then
          antinodesBuilder += pt
          recurse(Pt(pt.row + dr, pt.col + dc))

      recurse(p2)
    end addLineAntinodes

    for
      positions <- antennaGroups.values
      i <- positions.indices
      j <- 0 until i
      p1 = positions(i)
      p2 = positions(j)
    do
      addLineAntinodes(p1, p2)
      addLineAntinodes(p2, p1)

    val antinodes = antinodesBuilder.result()
    antinodes.size

