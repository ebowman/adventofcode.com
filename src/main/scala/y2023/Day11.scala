package y2023

trait Day11 {
  case class Point(x: Int, y: Int)
  case class Galaxy(id: Int, position: Point)

  def manhattanDistance(p1: Point, p2: Point): Int =
    Math.abs(p1.x - p2.x) + Math.abs(p1.y - p2.y)

  def findEmptyRowsAndCols(universe: List[String]): (Set[Int], Set[Int]) =
    val height = universe.length
    val width = universe.head.length

    val emptyRows = (0 until height).filter(y =>
      universe(y).forall(_ == '.')
    ).toSet

    val emptyCols = (0 until width).filter(x =>
      universe.forall(row => row(x) == '.')
    ).toSet

    (emptyRows, emptyCols)

  def findGalaxies(universe: List[String]): List[Galaxy] =
    var galaxyId = 1
    val galaxies = for {
      y <- universe.indices
      x <- universe(y).indices
      if universe(y)(x) == '#'
    } yield {
      val galaxy = Galaxy(galaxyId, Point(x, y))
      galaxyId += 1
      galaxy
    }
    galaxies.toList

  def calculateExpandedDistance(g1: Galaxy, g2: Galaxy,
                                emptyRows: Set[Int], emptyCols: Set[Int],
                                expansionFactor: Long = 2): Long =
    val minX = math.min(g1.position.x, g2.position.x)
    val maxX = math.max(g1.position.x, g2.position.x)
    val minY = math.min(g1.position.y, g2.position.y)
    val maxY = math.max(g1.position.y, g2.position.y)

    val crossedEmptyRows = emptyRows.count(y => y > minY && y < maxY)
    val crossedEmptyCols = emptyCols.count(x => x > minX && x < maxX)

    // Base manhattan distance plus additional space from expansion
    // Each empty row/col adds (expansionFactor - 1) additional spaces
    manhattanDistance(g1.position, g2.position) +
      (crossedEmptyRows + crossedEmptyCols) * (expansionFactor - 1)

  def solvePart1(input: Seq[String]): Long = solvePart2(input, 2)

  def solvePart2(input: Seq[String], scale: Int=1000000): Long =
    val universe = input.toList
    val (emptyRows, emptyCols) = findEmptyRowsAndCols(universe)
    val galaxies = findGalaxies(universe)

    // Generate all unique pairs of galaxies
    val galaxyPairs = for {
      i <- galaxies.indices
      j <- (i + 1) until galaxies.length
    } yield (galaxies(i), galaxies(j))

    // Calculate and sum all distances with million times expansion
    galaxyPairs.map { case (g1, g2) =>
      calculateExpandedDistance(g1, g2, emptyRows, emptyCols, scale)
    }.sum
}