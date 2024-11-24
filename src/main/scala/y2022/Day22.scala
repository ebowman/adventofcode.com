package y2022

trait Day22:
  enum Direction:
    case Right, Down, Left, Up

    def turn(c: Char): Direction =
      val delta = if c == 'R' then 1 else -1
      Direction.fromOrdinal((ordinal + delta + 4) % 4)

    def value: Int = ordinal

  private case class Position(row: Int, col: Int)

  private case class BoardState(pos: Position, dir: Direction)

  private def parseInput(input: Seq[String]): (Array[Array[Char]], String) =
    val (mapLines, movesLines) = input.span(_.nonEmpty)
    val width = mapLines.map(_.length).max
    val map = mapLines.map(_.padTo(width, ' ').toArray).toArray
    val moves = movesLines.last.trim
    (map, moves)

  private def parseMoves(moves: String): List[Either[Int, Char]] =
    val pattern = """(\d+|[RL])""".r
    pattern.findAllIn(moves).toList.map {
      case s if s.forall(_.isDigit) => Left(s.toInt)
      case s => Right(s.head)
    }

  private def findStartPosition(map: Array[Array[Char]]): Position =
    Position(0, map(0).indexWhere(_ == '.'))

  private def wrapPosition(map: Array[Array[Char]], pos: Position, dir: Direction): Position =
    dir match
      case Direction.Right =>
        val row = pos.row
        var col = 0
        while col < map(row).length && map(row)(col) == ' ' do col += 1
        Position(row, col)

      case Direction.Left =>
        val row = pos.row
        var col = map(row).length - 1
        while col >= 0 && map(row)(col) == ' ' do col -= 1
        Position(row, col)

      case Direction.Down =>
        val col = pos.col
        var row = 0
        while row < map.length && (col >= map(row).length || map(row)(col) == ' ') do row += 1
        Position(row, col)

      case Direction.Up =>
        val col = pos.col
        var row = map.length - 1
        while row >= 0 && (col >= map(row).length || map(row)(col) == ' ') do row -= 1
        Position(row, col)
  end wrapPosition

  private def moveInDirection(map: Array[Array[Char]], state: BoardState, steps: Int): BoardState =
    def isValidPosition(pos: Position): Boolean =
      pos.row >= 0 && pos.row < map.length &&
        pos.col >= 0 && pos.col < map(pos.row).length &&
        map(pos.row)(pos.col) != ' '

    def nextPosition(pos: Position, dir: Direction): Position =
      val (dr, dc) = dir match
        case Direction.Right => (0, 1)
        case Direction.Down => (1, 0)
        case Direction.Left => (0, -1)
        case Direction.Up => (-1, 0)

      val newRow = pos.row + dr
      val newCol = pos.col + dc

      if !isValidPosition(Position(newRow, newCol)) then
        wrapPosition(map, pos, dir)
      else
        Position(newRow, newCol)
    end nextPosition

    @annotation.tailrec
    def move(state: BoardState, remaining: Int): BoardState =
      if remaining == 0 then state
      else
        val nextPos = nextPosition(state.pos, state.dir)
        if map(nextPos.row)(nextPos.col) == '#' then
          state
        else
          move(BoardState(nextPos, state.dir), remaining - 1)
    end move

    move(state, steps)

  def solvePart1(input: Seq[String]): Int =
    val (map, movesString) = parseInput(input)
    val moves = parseMoves(movesString)

    val initialState = BoardState(findStartPosition(map), Direction.Right)

    val finalState = moves.foldLeft(initialState):
      case (state, move) =>
        move match
          case Left(steps) => moveInDirection(map, state, steps)
          case Right(turn) => BoardState(state.pos, state.dir.turn(turn))

    1000 * (finalState.pos.row + 1) +
      4 * (finalState.pos.col + 1) +
      finalState.dir.value
  end solvePart1

  private case class Face(id: Int, topLeft: Position, size: Int)

  private def identifyCubeFaces(map: Array[Array[Char]]): Map[Int, Face] =
    val size = if map.length > 50 then 50 else 4

    val faces = for
      row <- map.indices by size
      col <- map(row).indices by size
      if col < map(row).length && map(row)(col) != ' '
    yield
      val faceNum = if size == 50 then
        (row / size, col / size) match
          case (0, 1) => 1
          case (0, 2) => 2
          case (1, 1) => 3
          case (2, 0) => 4
          case (2, 1) => 5
          case (3, 0) => 6
          case _ => 0
      else
        (row / size, col / size) match
          case (0, 2) => 1
          case (1, 0) => 2
          case (1, 1) => 3
          case (1, 2) => 4
          case (2, 2) => 5
          case (2, 3) => 6
          case _ => 0

      faceNum -> Face(faceNum, Position(row, col), size)

    faces.toMap
  end identifyCubeFaces

  private def getNextCubePosition(faces: Map[Int, Face], state: BoardState): BoardState =
    val size = faces.values.head.size
    val currentFace = faces.values.find(face =>
      state.pos.row >= face.topLeft.row &&
        state.pos.row < face.topLeft.row + size &&
        state.pos.col >= face.topLeft.col &&
        state.pos.col < face.topLeft.col + size
    ).get

    val localRow = state.pos.row - currentFace.topLeft.row
    val localCol = state.pos.col - currentFace.topLeft.col

    val nextPos = state.dir match
      case Direction.Right => Position(state.pos.row, state.pos.col + 1)
      case Direction.Down => Position(state.pos.row + 1, state.pos.col)
      case Direction.Left => Position(state.pos.row, state.pos.col - 1)
      case Direction.Up => Position(state.pos.row - 1, state.pos.col)

    if nextPos.row >= currentFace.topLeft.row &&
      nextPos.row < currentFace.topLeft.row + size &&
      nextPos.col >= currentFace.topLeft.col &&
      nextPos.col < currentFace.topLeft.col + size then
      BoardState(nextPos, state.dir)
    else
      val (newFaceId, newDir, newRow, newCol) =
        if size == 50 then
          (currentFace.id, state.dir) match
            // Face 1 transitions
            case (1, Direction.Up)    => (6, Direction.Right, localCol, 0)
            case (1, Direction.Right) => (2, Direction.Right, localRow, 0)
            case (1, Direction.Down)  => (3, Direction.Down, 0, localCol)
            case (1, Direction.Left)  => (4, Direction.Right, size - 1 - localRow, 0)
            // Face 2 transitions
            case (2, Direction.Up)    => (6, Direction.Up, size - 1, localCol)
            case (2, Direction.Right) => (5, Direction.Left, size - 1 - localRow, size - 1)
            case (2, Direction.Down)  => (3, Direction.Left, localCol, size - 1)
            case (2, Direction.Left)  => (1, Direction.Left, localRow, size - 1)
            // Face 3 transitions
            case (3, Direction.Up)    => (1, Direction.Up, size - 1, localCol)
            case (3, Direction.Right) => (2, Direction.Up, size - 1, localRow)
            case (3, Direction.Down)  => (5, Direction.Down, 0, localCol)
            case (3, Direction.Left)  => (4, Direction.Down, 0, localRow)
            // Face 4 transitions
            case (4, Direction.Up)    => (3, Direction.Right, localCol, 0)
            case (4, Direction.Right) => (5, Direction.Right, localRow, 0)
            case (4, Direction.Down)  => (6, Direction.Down, 0, localCol)
            case (4, Direction.Left)  => (1, Direction.Right, size - 1 - localRow, 0)
            // Face 5 transitions
            case (5, Direction.Up)    => (3, Direction.Up, size - 1, localCol)
            case (5, Direction.Right) => (2, Direction.Left, size - 1 - localRow, size - 1)
            case (5, Direction.Down)  => (6, Direction.Left, localCol, size - 1)
            case (5, Direction.Left)  => (4, Direction.Left, localRow, size - 1)
            // Face 6 transitions
            case (6, Direction.Up)    => (4, Direction.Up, size - 1, localCol)
            case (6, Direction.Right) => (5, Direction.Up, size - 1, localRow)
            case (6, Direction.Down)  => (2, Direction.Down, 0, localCol)
            case (6, Direction.Left)  => (1, Direction.Down, 0, localRow)
            case _ => throw new IllegalStateException("Invalid state")
        else
          (currentFace.id, state.dir) match
            // Face transitions for test input
            // Face 1
            case (1, Direction.Up)    => (2, Direction.Down, 0, size - 1 - localCol)
            case (1, Direction.Right) => (4, Direction.Right, localRow, 0)
            case (1, Direction.Down)  => (4, Direction.Down, 0, localCol)
            case (1, Direction.Left)  => (2, Direction.Down, 0, localRow)
            // Face 2
            case (2, Direction.Up)    => (1, Direction.Down, 0, size - 1 - localCol)
            case (2, Direction.Right) => (3, Direction.Right, localRow, 0)
            case (2, Direction.Down)  => (5, Direction.Up, size - 1, size - 1 - localCol)
            case (2, Direction.Left)  => (4, Direction.Up, size - 1, size - 1 - localRow)
            // Face 3
            case (3, Direction.Up)    => (1, Direction.Right, localCol, 0)
            case (3, Direction.Right) => (4, Direction.Right, localRow, 0)
            case (3, Direction.Down)  => (5, Direction.Right, localCol, 0)
            case (3, Direction.Left)  => (2, Direction.Left, localRow, size - 1)
            // Face 4
            case (4, Direction.Up)    => (1, Direction.Up, size - 1, localCol)
            case (4, Direction.Right) => (6, Direction.Down, 0, size - 1 - localRow)
            case (4, Direction.Down)  => (5, Direction.Down, 0, localCol)
            case (4, Direction.Left)  => (3, Direction.Left, localRow, size - 1)
            // Face 5
            case (5, Direction.Up)    => (4, Direction.Up, size - 1, localCol)
            case (5, Direction.Right) => (6, Direction.Right, localRow, 0)
            case (5, Direction.Down)  => (2, Direction.Up, size - 1, size - 1 - localCol)
            case (5, Direction.Left)  => (3, Direction.Up, size - 1, localCol)
            // Face 6
            case (6, Direction.Up)    => (4, Direction.Left, localCol, size - 1)
            case (6, Direction.Right) => (1, Direction.Left, size - 1 - localRow, size - 1)
            case (6, Direction.Down)  => (2, Direction.Right, localCol, 0)
            case (6, Direction.Left)  => (5, Direction.Left, localRow, size - 1)
            case _ => throw new IllegalStateException("Invalid state")

      val newFace = faces(newFaceId)
      BoardState(
        Position(newFace.topLeft.row + newRow, newFace.topLeft.col + newCol),
        newDir
      )
  end getNextCubePosition

  private def moveInDirectionCube(
                                   map: Array[Array[Char]],
                                   faces: Map[Int, Face],
                                   state: BoardState,
                                   steps: Int
                                 ): BoardState =
    @annotation.tailrec
    def move(state: BoardState, remaining: Int): BoardState =
      if remaining == 0 then state
      else
        val nextState = getNextCubePosition(faces, state)
        if map(nextState.pos.row)(nextState.pos.col) == '#' then
          state
        else
          move(nextState, remaining - 1)
    move(state, steps)

  def solvePart2(input: Seq[String]): Int =
    val (map, movesString) = parseInput(input)
    val moves = parseMoves(movesString)
    val faces = identifyCubeFaces(map)

    val initialState = BoardState(findStartPosition(map), Direction.Right)

    val finalState = moves.foldLeft(initialState):
      case (state, move) =>
        move match
          case Left(steps) => moveInDirectionCube(map, faces, state, steps)
          case Right(turn) => BoardState(state.pos, state.dir.turn(turn))

    1000 * (finalState.pos.row + 1) +
      4 * (finalState.pos.col + 1) +
      finalState.dir.value
end Day22