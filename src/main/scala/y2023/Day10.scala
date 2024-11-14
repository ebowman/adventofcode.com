package y2023

trait Day10 {
  private val pipeConnections = Map(
    '|' -> Set(Point(0, -1), Point(0, 1)),
    '-' -> Set(Point(-1, 0), Point(1, 0)),
    'L' -> Set(Point(0, -1), Point(1, 0)),
    'J' -> Set(Point(0, -1), Point(-1, 0)),
    '7' -> Set(Point(0, 1), Point(-1, 0)),
    'F' -> Set(Point(0, 1), Point(1, 0)),
    '.' -> Set(),
    'S' -> Set(Point(0, -1), Point(1, 0), Point(0, 1), Point(-1, 0))
  )

  private def isValidPoint(p: Point, grid: Array[Array[Char]]): Boolean =
    p.y >= 0 && p.y < grid.length && p.x >= 0 && p.x < grid(0).length

  def solvePart1(input: Seq[String]): Int =
    val grid = input.map(_.trim.toCharArray).toArray
    val distances = findLoop(grid)
    distances.values.max

  def solvePart2(input: Seq[String]): Int =
    val grid = input.map(_.trim.toCharArray).toArray
    val mainLoop = findLoop(grid).keySet
    countEnclosedTiles(grid, mainLoop)

  private def findLoop(grid: Array[Array[Char]]): Map[Point, Int] =
    val start = findStart(grid)
    var distances = Map(start -> 0)
    var queue = List((start, 0))

    while (queue.nonEmpty) {
      val (current, dist) = queue.head
      queue = queue.tail

      val connectedPoints = current.neighbors.filter(next =>
        isValidPoint(next, grid) && areConnected(current, next, grid)
      )

      for {
        next <- connectedPoints
        if !distances.contains(next)
      } {
        distances = distances + (next -> (dist + 1))
        queue = queue :+ (next, dist + 1)
      }
    }
    distances

  private def findStart(grid: Array[Array[Char]]): Point =
    val y = grid.indexWhere(_.contains('S'))
    val x = grid(y).indexOf('S')
    Point(x, y)

  private def areConnected(from: Point, to: Point, grid: Array[Array[Char]]): Boolean =
    if (!isValidPoint(from, grid) || !isValidPoint(to, grid)) false
    else
      val fromPipe = grid(from.y)(from.x)
      val toPipe = grid(to.y)(to.x)

      val diff = Point(to.x - from.x, to.y - from.y)
      val reverseDiff = Point(-diff.x, -diff.y)

      pipeConnections(fromPipe).contains(diff) && pipeConnections(toPipe).contains(reverseDiff)

  private def countEnclosedTiles(grid: Array[Array[Char]], mainLoop: Set[Point]): Int = {
    var count = 0

    // Replace S with the correct pipe type
    val start = findStart(grid)
    val startConnections = start.neighbors.filter(n =>
      isValidPoint(n, grid) && areConnected(start, n, grid)
    ).map(n => Point(n.x - start.x, n.y - start.y)).toSet

    val startPipe = pipeConnections.find(_._2 == startConnections).map(_._1).getOrElse('|')

    // Count enclosed tiles using ray casting algorithm
    for (y <- grid.indices) {
      var inside = false
      var lastCorner: Option[Char] = None

      for (x <- grid(0).indices) {
        val point = Point(x, y)
        if (mainLoop.contains(point)) {
          val pipe = if (point == start) startPipe else grid(y)(x)
          pipe match {
            case '|' => inside = !inside
            case 'F' => lastCorner = Some('F')
            case 'L' => lastCorner = Some('L')
            case '7' =>
              lastCorner match {
                case Some('L') => inside = !inside
                case _ =>
              }
              lastCorner = None
            case 'J' =>
              lastCorner match {
                case Some('F') => inside = !inside
                case _ =>
              }
              lastCorner = None
            case _ => // Skip horizontal pipes
          }
        } else if (inside && !mainLoop.contains(point)) {
          count += 1
        }
      }
    }
    count
  }

  case class Point(x: Int, y: Int) {
    def neighbors: List[Point] = List(
      Point(0, -1),
      Point(1, 0),
      Point(0, 1),
      Point(-1, 0)
    ).map(this + _)

    def +(other: Point): Point = Point(x + other.x, y + other.y)
  }
}