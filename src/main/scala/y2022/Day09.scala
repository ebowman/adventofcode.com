package y2022


trait Day09:
  enum Direction:
    case Up, Down, Left, Right

  case class Position(x: Int, y: Int):
    def move(direction: Direction): Position = direction match
      case Direction.Up => copy(y = y + 1)
      case Direction.Down => copy(y = y - 1)
      case Direction.Left => copy(x = x - 1)
      case Direction.Right => copy(x = x + 1)

    def moveTowards(other: Position): Position =
      val dx = other.x - x
      val dy = other.y - y

      if math.abs(dx) <= 1 && math.abs(dy) <= 1 then
        this // Adjacent or overlapping - don't move
      else if math.abs(dx) >= 2 && math.abs(dy) >= 2 then
        // Move diagonally
        Position(x + dx.sign, y + dy.sign)
      else if math.abs(dx) >= 2 then
        // Move horizontally and match y
        Position(x + dx.sign, other.y)
      else if math.abs(dy) >= 2 then
        // Move vertically and match x
        Position(other.x, y + dy.sign)
      else
        // Move diagonally
        Position(x + dx.sign, y + dy.sign)

  case class Move(direction: Direction, steps: Int)

  private def parseDirection(s: String): Direction = s match
    case "U" => Direction.Up
    case "D" => Direction.Down
    case "L" => Direction.Left
    case "R" => Direction.Right
    case _ => throw IllegalArgumentException(s"Invalid direction: $s")

  private def parseMoves(input: Seq[String]): Seq[Move] =
    input.map: line =>
      val Array(dir, steps) = line.split(" ")
      Move(parseDirection(dir), steps.toInt)

  private def simulateRope(moves: Seq[Move], ropeLength: Int): Int =
    def executeMove(positions: Seq[Position], visited: Set[Position], move: Move): (Seq[Position], Set[Position]) =
      (1 to move.steps).foldLeft((positions, visited)):
        case ((currentPositions, currentVisited), _) =>
          val newHead = currentPositions.head.move(move.direction)
          val newPositions = currentPositions.tail.foldLeft(List(newHead)): (acc, knot) =>
            acc :+ knot.moveTowards(acc.last)
          (newPositions, currentVisited + newPositions.last)

    val initialPositions = Seq.fill(ropeLength)(Position(0, 0))
    val (_, tailVisited) = moves.foldLeft((initialPositions, Set(Position(0, 0)))):
      case ((positions, visited), move) => executeMove(positions, visited, move)

    tailVisited.size

  def solve(input: Seq[String], ropeLength: Int): Int = simulateRope(parseMoves(input), ropeLength)

end Day09
