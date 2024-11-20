package y2023

// see https://adventofcode.com/2023/day/23
trait Day23:
  enum Cell:
    case Path, Forest, SlopeUp, SlopeRight, SlopeDown, SlopeLeft

  case class Pos(row: Int, col: Int)
  case class Edge(dest: Pos, len: Int)

  def parseMap(input: Seq[String]): Vector[Vector[Cell]] =
    input.map: line =>
      line.map:
        case '.' => Cell.Path
        case '#' => Cell.Forest
        case '^' => Cell.SlopeUp
        case '>' => Cell.SlopeRight
        case 'v' => Cell.SlopeDown
        case '<' => Cell.SlopeLeft
      .toVector
    .toVector

  def findLongestPath(grid: Vector[Vector[Cell]], ignoreSlopes: Boolean = false): Int =
    val start = Pos(0, grid(0).indexWhere(_ == Cell.Path))
    val end = Pos(grid.size - 1, grid.last.indexWhere(_ == Cell.Path))

    def isValid(pos: Pos): Boolean =
      pos.row >= 0 && pos.row < grid.size &&
        pos.col >= 0 && pos.col < grid(0).size &&
        grid(pos.row)(pos.col) != Cell.Forest

    def buildGraph(): Map[Pos, List[Edge]] =
      def isIntersection(p: Pos): Boolean =
        val neighbors = List(
          Pos(p.row - 1, p.col), Pos(p.row + 1, p.col),
          Pos(p.row, p.col - 1), Pos(p.row, p.col + 1)
        ).count(n => isValid(n))
        neighbors > 2 || p == start || p == end

      def findNextIntersection(from: Pos, current: Pos, seen: Set[Pos]): Option[Edge] =
        if seen.contains(current) then None
        else if current != from && isIntersection(current) then Some(Edge(current, seen.size))
        else
          val nextSteps = List(
            Pos(current.row - 1, current.col), Pos(current.row + 1, current.col),
            Pos(current.row, current.col - 1), Pos(current.row, current.col + 1)
          ).filter(p => isValid(p) && !seen.contains(p))

          nextSteps.flatMap(next => findNextIntersection(from, next, seen + current)).headOption

      val intersections = for
        r <- grid.indices
        c <- grid(0).indices
        if grid(r)(c) != Cell.Forest && isIntersection(Pos(r, c))
      yield Pos(r, c)

      intersections.map: pos =>
        val edges = List(
          Pos(pos.row - 1, pos.col), Pos(pos.row + 1, pos.col),
          Pos(pos.row, pos.col - 1), Pos(pos.row, pos.col + 1)
        ).filter(isValid)
          .flatMap(next => findNextIntersection(pos, next, Set(pos)))
        pos -> edges
      .toMap

    if ignoreSlopes then
      val graph = buildGraph()

      def iterativeDFS(): Int =
        case class State(pos: Pos, visited: Set[Pos], length: Int)
        var maxLength = Int.MinValue
        val stack = collection.mutable.Stack[State]()
        stack.push(State(start, Set(start), 0))

        while stack.nonEmpty do
          val State(pos, visited, length) = stack.pop()
          if pos == end then
            maxLength = maxLength.max(length)
          else
            graph.getOrElse(pos, List.empty)
              .filterNot(edge => visited.contains(edge.dest))
              .foreach(edge =>
                stack.push(State(edge.dest, visited + edge.dest, length + edge.len)))

        maxLength

      iterativeDFS()
    else
      def iterativeDFS(): Int =
        case class State(pos: Pos, visited: Set[Pos])
        var maxLength = Int.MinValue
        val stack = collection.mutable.Stack[State]()
        stack.push(State(start, Set(start)))

        while stack.nonEmpty do
          val State(pos, visited) = stack.pop()
          if pos == end then
            maxLength = maxLength.max(visited.size - 1)
          else
            val moves = grid(pos.row)(pos.col) match
              case Cell.SlopeUp => List(Pos(pos.row - 1, pos.col))
              case Cell.SlopeRight => List(Pos(pos.row, pos.col + 1))
              case Cell.SlopeDown => List(Pos(pos.row + 1, pos.col))
              case Cell.SlopeLeft => List(Pos(pos.row, pos.col - 1))
              case _ => List(
                Pos(pos.row - 1, pos.col), Pos(pos.row + 1, pos.col),
                Pos(pos.row, pos.col - 1), Pos(pos.row, pos.col + 1)
              )

            moves.filter(isValid)
              .filterNot(visited.contains)
              .foreach(next =>
                stack.push(State(next, visited + next)))

        maxLength

      iterativeDFS()

  def solvePart1(input: Seq[String]): Int =
    val grid = parseMap(input)
    findLongestPath(grid)

  def solvePart2(input: Seq[String]): Int =
    val grid = parseMap(input)
    findLongestPath(grid, ignoreSlopes = true)
end Day23