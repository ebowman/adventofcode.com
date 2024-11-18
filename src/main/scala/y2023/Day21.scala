package y2023

// see https://adventofcode.com/2023/day/21
// solution inspired by https://github.com/villuna/aoc23/wiki/A-Geometric-solution-to-advent-of-code-2023,-day-21
trait Day21:
  def solvePart1(input: Seq[String], steps: Int): Int =
    Garden(input.map(_.trim.toVector).toVector).countReachablePlots(steps)

  def solvePart2(input: Seq[String], steps: Long): Long =
    Garden(input.map(_.trim.toVector).toVector).countInfiniteReachablePlots(steps)

  case class Pos(row: Int, col: Int):
    def +(d: Pos): Pos = Pos(row + d.row, col + d.col)

  case class Garden(grid: Vector[Vector[Char]]):
    private val directions = List(
      Pos(-1, 0), // up
      Pos(1, 0),  // down
      Pos(0, -1), // left
      Pos(0, 1)   // right
    )

    def countReachablePlots(steps: Int): Int =
      val start = findStart.getOrElse(throw new IllegalStateException("No starting position found"))
      val distances = bfs(start)
      distances.values.count(d => d <= steps && d % 2 == steps % 2)

    def countInfiniteReachablePlots(steps: Long): Long =
      val start = findStart.getOrElse(throw new IllegalStateException("No starting position found"))

      // Only use geometric solution for the actual puzzle input which has specific properties
      if width != 131 || steps < 1000 then
        // For test cases and small steps, use brute force BFS with wrap-around
        bfsInfinite(start, steps.toInt)
      else
        // For the actual puzzle with size 131 and steps 26501365
        val distances = bfs(start)
        val size = width
        val radius = size / 2
        val n = ((steps - radius) / size).toLong  // Should be 202300 for actual input

        // Count points based on their characteristics
        val evenFull = distances.values.count(d => d % 2 == 0).toLong
        val oddFull = distances.values.count(d => d % 2 == 1).toLong
        val evenCorners = distances.values.count(d => d % 2 == 0 && d > radius).toLong
        val oddCorners = distances.values.count(d => d % 2 == 1 && d > radius).toLong

        ((n + 1L) * (n + 1L) * oddFull) +  // Odd filled squares
          (n * n * evenFull) +               // Even filled squares
          (-((n + 1L) * oddCorners)) +      // Remove odd corners
          (n * evenCorners)                  // Add even corners

    private def bfsInfinite(start: Pos, steps: Int): Long =
      val seen = collection.mutable.Set[(Pos, Int, Int)]()  // pos, gridRow, gridCol
      val counts = collection.mutable.Map[(Int, Int), Int]().withDefaultValue(0)
      val queue = collection.mutable.Queue[(Pos, Int, Int, Int)]()  // pos, gridRow, gridCol, dist

      queue.enqueue((start, 0, 0, 0))

      while queue.nonEmpty do
        val (pos, gridRow, gridCol, dist) = queue.dequeue()
        val key = (pos, gridRow, gridCol)

        if !seen(key) && dist <= steps then
          seen += key

          if dist % 2 == steps % 2 then
            counts(gridRow -> gridCol) += 1

          if dist < steps then
            for d <- directions do
              var newPos = pos + d
              var newGridRow = gridRow
              var newGridCol = gridCol

              if newPos.row < 0 then
                newPos = Pos(height - 1, newPos.col)
                newGridRow -= 1
              else if newPos.row >= height then
                newPos = Pos(0, newPos.col)
                newGridRow += 1

              if newPos.col < 0 then
                newPos = Pos(newPos.row, width - 1)
                newGridCol -= 1
              else if newPos.col >= width then
                newPos = Pos(newPos.row, 0)
                newGridCol += 1

              if isGardenPlot(newPos) then
                queue.enqueue((newPos, newGridRow, newGridCol, dist + 1))

      counts.values.map(_.toLong).sum

    private def bfs(start: Pos): Map[Pos, Int] =
      val distances = collection.mutable.Map[Pos, Int]()
      val queue = collection.mutable.Queue((start, 0))
      distances(start) = 0

      while queue.nonEmpty do
        val (pos, dist) = queue.dequeue()

        for
          d <- directions
          next = pos + d
          if next.row >= 0 && next.row < height &&
            next.col >= 0 && next.col < width &&
            isGardenPlot(next) &&
            !distances.contains(next)
        do
          distances(next) = dist + 1
          queue.enqueue((next, dist + 1))

      distances.toMap

    def isGardenPlot(pos: Pos): Boolean =
      pos.row >= 0 && pos.row < height &&
        pos.col >= 0 && pos.col < width &&
        (grid(pos.row)(pos.col) == '.' || grid(pos.row)(pos.col) == 'S')

    def width: Int = if grid.isEmpty then 0 else grid(0).length
    def height: Int = grid.length

    def findStart: Option[Pos] =
      for
        row <- grid.indices.find(r => grid(r).contains('S'))
        col = grid(row).indexOf('S')
      yield Pos(row, col)
end Day21