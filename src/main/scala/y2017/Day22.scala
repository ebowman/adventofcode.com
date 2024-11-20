package y2017

trait Day22:
  enum Direction:
    case Up, Right, Down, Left

    def turnLeft: Direction = this match
      case Up => Left
      case Left => Down
      case Down => Right
      case Right => Up

    def turnRight: Direction = this match
      case Up => Right
      case Right => Down
      case Down => Left
      case Left => Up

    def reverse: Direction = this match
      case Up => Down
      case Down => Up
      case Left => Right
      case Right => Left

  enum NodeState:
    case Clean, Weakened, Infected, Flagged

  case class Position(x: Int, y: Int):
    def move(dir: Direction): Position = dir match
      case Direction.Up => copy(y = y - 1)
      case Direction.Down => copy(y = y + 1)
      case Direction.Left => copy(x = x - 1)
      case Direction.Right => copy(x = x + 1)

  def solvePart1(input: Seq[String]): Int =
    var grid = parseInput(input, true)
    var pos = Position(0, 0)
    var dir = Direction.Up
    var infectionCount = 0

    for _ <- 1 to 10_000 do
      val isInfected = grid.getOrElse(pos, false)
      dir = if isInfected then dir.turnRight else dir.turnLeft

      if isInfected then
        grid = grid - pos
      else
        grid = grid + (pos -> true)
        infectionCount += 1

      pos = pos.move(dir)

    infectionCount

  def solvePart2(input: Seq[String]): Int =
    var grid = parseInput(input, NodeState.Infected)
    var pos = Position(0, 0)
    var dir = Direction.Up
    var infectionCount = 0

    for _ <- 1 to 10_000_000 do
      val currentState = grid.getOrElse(pos, NodeState.Clean)

      // Update direction based on current node state
      dir = currentState match
        case NodeState.Clean => dir.turnLeft
        case NodeState.Weakened => dir
        case NodeState.Infected => dir.turnRight
        case NodeState.Flagged => dir.reverse

      // Update node state
      val newState = currentState match
        case NodeState.Clean =>
          NodeState.Weakened
        case NodeState.Weakened =>
          infectionCount += 1
          NodeState.Infected
        case NodeState.Infected =>
          NodeState.Flagged
        case NodeState.Flagged =>
          NodeState.Clean

      // Update grid
      if newState == NodeState.Clean then
        grid = grid - pos
      else
        grid = grid + (pos -> newState)

      pos = pos.move(dir)

    infectionCount

  private def parseInput[T](input: Seq[String], defaultValue: T): Map[Position, T] =
    val height = input.size
    val width = input.head.length
    val startY = height / 2
    val startX = width / 2

    (for
      y <- input.indices
      x <- input(y).indices
      if input(y)(x) == '#'
    yield Position(x - startX, y - startY) -> defaultValue).toMap

end Day22