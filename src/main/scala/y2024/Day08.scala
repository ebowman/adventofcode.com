package y2024

import scala.annotation.tailrec

class Day08 extends util.Day(8):

  private case class Pt(row: Int, col: Int):
    def isInBounds(height: Int, width: Int): Boolean =
      row >= 0 && row < height && col >= 0 && col < width

  private def parseInput(input: IndexedSeq[String]) =
    val height = input.length
    val width = input.head.length
    val builder = Map.newBuilder[Char, List[Pt]]

    for
      (line, row) <- input.zipWithIndex
      (char, col) <- line.zipWithIndex
      if char != '.'
    do
      val existing = builder.result().getOrElse(char, Nil)
      builder.addOne(char -> (Pt(row, col) :: existing))

    val antennaGroups = builder.result()
    (height, width, antennaGroups)

  def solvePart1(input: IndexedSeq[String]): Int =
    val (height, width, antennaGroups) = parseInput(input)
    val builder = Set.newBuilder[Pt]

    for
      positions <- antennaGroups.values
      i <- positions.indices
      j <- 0 until i
      p1 = positions(i)
      p2 = positions(j)
      dr = p2.row - p1.row
      dc = p2.col - p1.col
      forward = Pt(p2.row + dr, p2.col + dc)
      reverse = Pt(p1.row - dr, p1.col - dc)
    do
      if forward.isInBounds(height, width) then builder += forward
      if reverse.isInBounds(height, width) then builder += reverse

    val antinodes = builder.result()
    antinodes.size

  def solvePart2(input: IndexedSeq[String]): Int =
    val (height, width, antennaGroups) = parseInput(input)
    val builder = Set.newBuilder[Pt]

    def addLineAntinodes(p1: Pt, p2: Pt): Unit =
      val dr = p2.row - p1.row
      val dc = p2.col - p1.col

      @tailrec
      def recurse(pt: Pt): Unit =
        if pt.isInBounds(height, width) then
          builder += pt
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

    val antinodes = builder.result()
    antinodes.size

