package y2022

trait Day08:
  def solvePart1(input: Seq[String]): Int =
    val (grid, height, width) = parseInput(input)

    def isVisible(row: Int, col: Int): Boolean =
      if row == 0 || col == 0 || row == height - 1 || col == width - 1 then
        true
      else
        val treeHeight = grid(row)(col)

        val visibleFromLeft = (0 until col).forall(c => grid(row)(c) < treeHeight)
        val visibleFromRight = (col + 1 until width).forall(c => grid(row)(c) < treeHeight)
        val visibleFromTop = (0 until row).forall(r => grid(r)(col) < treeHeight)
        val visibleFromBottom = (row + 1 until height).forall(r => grid(r)(col) < treeHeight)

        visibleFromLeft || visibleFromRight || visibleFromTop || visibleFromBottom

    val positions = for
      row <- 0 until height
      col <- 0 until width
      if isVisible(row, col)
    yield (row, col)

    positions.size

  def parseInput(input: Seq[String]): (Vector[Vector[Int]], Int, Int) =
    val grid = input.map(_.map(_.asDigit).toVector).toVector
    val height = grid.length
    val width = grid(0).length
    (grid, height, width)

  def solvePart2(input: Seq[String]): Int =
    val (grid, height, width) = parseInput(input)

    def countVisibleTrees(row: Int, col: Int): Int =
      val treeHeight = grid(row)(col)

      def lookDirection(positions: Seq[(Int, Int)]): Int =
        val visibleTrees = positions.takeWhile((r, c) => grid(r)(c) < treeHeight).length
        val blockedByTree = positions.exists((r, c) => grid(r)(c) >= treeHeight)
        if blockedByTree then visibleTrees + 1 else visibleTrees

      // Generate sequences of positions in each direction
      val leftPositions = (col - 1 to 0 by -1).map((row, _))
      val rightPositions = (col + 1 until width).map((row, _))
      val upPositions = (row - 1 to 0 by -1).map((_, col))
      val downPositions = (row + 1 until height).map((_, col))

      // Calculate viewing distance in each direction
      val left = if col == 0 then 0 else lookDirection(leftPositions)
      val right = if col == width - 1 then 0 else lookDirection(rightPositions)
      val up = if row == 0 then 0 else lookDirection(upPositions)
      val down = if row == height - 1 then 0 else lookDirection(downPositions)

      left * right * up * down

    val scenicScores = for
      row <- 0 until height
      col <- 0 until width
    yield countVisibleTrees(row, col)

    scenicScores.max
end Day08
