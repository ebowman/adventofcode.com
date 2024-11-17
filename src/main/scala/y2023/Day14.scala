package y2023

// see https://adventofcode.com/2023/day/14
trait Day14 {
  def solvePart1(input: Seq[String]): Int =
    val grid = input.map(_.toCharArray).toArray
    tilt(grid, Direction.North)
    calculateLoad(grid)

  private def tilt(grid: Array[Array[Char]], direction: Direction): Unit =
    val rows = grid.length
    val cols = grid(0).length

    val (rangeRow, rangeCol) = direction match
      case Direction.North => (0 until rows, 0 until cols)
      case Direction.South => ((rows - 1) to 0 by -1, 0 until cols)
      case Direction.West => (0 until rows, 0 until cols)
      case Direction.East => (0 until rows, (cols - 1) to 0 by -1)

    for
      row <- rangeRow
      col <- rangeCol
      if grid(row)(col) == 'O'
    do
      val (newRow, newCol) = direction match
        case Direction.North => moveRock(grid, row, col, -1, 0)
        case Direction.South => moveRock(grid, row, col, 1, 0)
        case Direction.West => moveRock(grid, row, col, 0, -1)
        case Direction.East => moveRock(grid, row, col, 0, 1)

      if (newRow != row || newCol != col) then
        grid(newRow)(newCol) = 'O'
        grid(row)(col) = '.'

  private def moveRock(grid: Array[Array[Char]], row: Int, col: Int, dRow: Int, dCol: Int): (Int, Int) =
    var (newRow, newCol) = (row, col)
    while
      val nextRow = newRow + dRow
      val nextCol = newCol + dCol
      nextRow >= 0 && nextRow < grid.length &&
        nextCol >= 0 && nextCol < grid(0).length &&
        grid(nextRow)(nextCol) == '.'
    do
      newRow += dRow
      newCol += dCol
    (newRow, newCol)

  private def calculateLoad(grid: Array[Array[Char]]): Int =
    val rows = grid.length
    grid.indices.map(row =>
      grid(row).count(_ == 'O') * (rows - row)
    ).sum

  def solvePart2(input: Seq[String]): Int =
    val grid = input.map(_.toCharArray).toArray
    val seen = scala.collection.mutable.Map[String, Int]()
    var cycle = 0
    val targetCycles = 1000000000

    while cycle < targetCycles do
      val state = gridToString(grid)
      seen.get(state) match
        case Some(prevCycle) =>
          val cycleLength = cycle - prevCycle
          val remaining = (targetCycles - cycle) % cycleLength
          (0 until remaining).foreach(_ => performCycle(grid))
          return calculateLoad(grid)
        case None =>
          seen(state) = cycle
          performCycle(grid)
          cycle += 1

    calculateLoad(grid)

  private def performCycle(grid: Array[Array[Char]]): Unit =
    // Must follow exactly: north, west, south, east
    Direction.values.foreach(tilt(grid, _))

  private def gridToString(grid: Array[Array[Char]]): String =
    grid.map(_.mkString).mkString("\n")

  private enum Direction:
    case North, West, South, East // Order matters for the cycle!
}