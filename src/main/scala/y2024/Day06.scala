package y2024

import scala.annotation.tailrec

class Day06 extends util.Day(6):

  enum Direction:
    case Up, Right, Down, Left

    def turnRight: Direction = this match
      case Up    => Right
      case Right => Down
      case Down  => Left
      case Left  => Up

  case class Position(x: Int, y: Int)
  case class State(pos: Position, dir: Direction)

  private val directionDeltas: Map[Direction, (Int, Int)] = Map(
    Direction.Up    -> (0, -1),
    Direction.Right -> (1, 0),
    Direction.Down  -> (0, 1),
    Direction.Left  -> (-1, 0)
  )

  def parseInput(input: IndexedSeq[String]): (Set[Position], State) =
    val parsed: Seq[(Position, Char)] = for
      y <- input.indices
      x <- input(0).indices
      char = input(y)(x)
      pos = Position(x, y)
    yield (pos, char)

    val (startStateOpt, obstacles) = parsed.foldLeft((Option.empty[State], Set.empty[Position])):
      case ((maybeStart, obs), (pos, char)) =>
        char match
          case '^' => (Some(State(pos, Direction.Up)), obs)
          case '>' => (Some(State(pos, Direction.Right)), obs)
          case 'v' => (Some(State(pos, Direction.Down)), obs)
          case '<' => (Some(State(pos, Direction.Left)), obs)
          case '#' => (maybeStart, obs + pos)
          case '.' => (maybeStart, obs)

    val startState = startStateOpt.getOrElse:
      throw new IllegalArgumentException("No start state found")

    (obstacles - startState.pos, startState)  // start pos is not an obstacle! (argh)
  end parseInput

  def isInBounds(pos: Position, width: Int, height: Int): Boolean =
    pos.x >= 0 && pos.x < width && pos.y >= 0 && pos.y < height

  def advance(state: State, obstacleSet: Set[Position], width: Int, height: Int): (State, Boolean) =
    val (dx, dy) = directionDeltas(state.dir)
    val nextPos = Position(state.pos.x + dx, state.pos.y + dy)

    if !isInBounds(nextPos, width, height) then
      (state, false)
    else if obstacleSet.contains(nextPos) then
      (state.copy(dir = state.dir.turnRight), true)
    else
      (state.copy(pos = nextPos), true)

  private def runSimulation(startState: State,
                            obstacleSet: Set[Position],
                            width: Int,
                            height: Int): (Set[Position], Boolean) =

    @tailrec
    def recurse(state: State,
                visited: Set[Position],
                stateHistory: Map[State, Int],
                steps: Int): (Set[Position], Boolean) =
      val updatedVisited = visited + state.pos
      if stateHistory.contains(state) then
        (updatedVisited, true)
      else
        val (nextState, shouldContinue) = advance(state, obstacleSet, width, height)
        if !shouldContinue then
          (updatedVisited, false)
        else
          recurse(nextState, updatedVisited, stateHistory + (state -> steps), steps + 1)
    end recurse

    recurse(startState, Set.empty, Map.empty, 0)
  end runSimulation

  def solvePart1(input: IndexedSeq[String]): Int =
    val (obstacles, startState) = parseInput(input)
    val (width, height) = (input.head.length, input.size)

    val (visited, _) = runSimulation(startState, obstacles, width, height)
    visited.size
  end solvePart1

  def solvePart2(input: IndexedSeq[String]): Int =
    val (originalObstacles, startState) = parseInput(input)
    val (width, height) = (input.head.length, input.size)

    def createsLoop(extraObstacle: Position): Boolean =
      if !isInBounds(extraObstacle, width, height)
        || extraObstacle == startState.pos
        || originalObstacles.contains(extraObstacle)
      then
        false
      else
        val obstacleSet = originalObstacles + extraObstacle
        val (_, loopFound) = runSimulation(startState, obstacleSet, width, height)
        loopFound

    (for {
      y <- 0 until height
      x <- 0 until width
      pos = Position(x, y)
    } yield pos).count(createsLoop)

  end solvePart2
end Day06
