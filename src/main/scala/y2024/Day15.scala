package y2024

import scala.annotation.{tailrec, targetName}

class Day15 extends util.Day(15):
  type Grid = Vector[Vector[Char]]
  private val directionMap: Map[Char, Pos] = Map(
    '^' -> Pos(0, -1),
    'v' -> Pos(0, 1),
    '<' -> Pos(-1, 0),
    '>' -> Pos(1, 0)
  )

  private val boxChars = Set('O', '[', ']')
  private val blockedChars = Set('#', 'O', '[', ']')

  def solvePart1(input: IndexedSeq[String]): Int =
    solve(input, 'O')

  def solvePart2(input: IndexedSeq[String]): Int =
    solve(input, '[', scaleGrid)

  private def solve(input: IndexedSeq[String], matchChar: Char, tf: Grid => Grid = identity): Int =
    parseInput(input, tf)
      .map:
        case (grid, robotPos, moves) => runAllMoves(grid, robotPos, moves)
      .map: g =>
        sumBoxGPS(g, matchChar)
      .getOrElse(sys.error("No solution"))

  def parseInput(input: IndexedSeq[String], tf: Grid => Grid = identity): Option[(Grid, Pos, Vector[Pos])] =
    val (gridLines, moveLines) = input.span(_.nonEmpty)
    val grid = tf(gridLines.map(_.toVector).toVector)
    for
      robotPos <- findRobot(grid)
      moves = moveLines.drop(1).mkString("").collect:
        case c if directionMap.contains(c) => directionMap(c)
      .toVector
    yield (grid, robotPos, moves)

  private def findRobot(grid: Grid): Option[Pos] =
    for
      y <- grid.indices.find(y => grid(y).contains('@'))
      x <- grid(y).indexWhere(_ == '@') match
        case -1 => None
        case px => Some(px)
    yield Pos(x, y)

  private def runAllMoves(grid: Grid, start: Pos, moves: Vector[Pos]): Grid =
    @tailrec
    def recurse(state: (Grid, Pos), remaining: Vector[Pos]): Grid = remaining match
      case move +: rest =>
        val newState = attemptMove(state._1, state._2, move)
        recurse(newState, rest)
      case _ => state._1

    recurse((grid, start), moves)

  private def attemptMove(grid: Grid, robot: Pos, move: Pos): (Grid, Pos) =
    val target = robot + move
    val currentCell = cellAt(grid, target)
    if !inBounds(grid, target) || currentCell == '#' then
      (grid, robot)
    else
      currentCell match
        case c if isFloorLike(c) =>
          (moveEntity(grid, robot, target), target)

        case c if boxChars.contains(c) =>
          val (canPush, boxes) = canPushAll(grid, target, move)
          if canPush then
            val newGrid = moveEntity(moveBoxes(grid, boxes, move), robot, target)
            (newGrid, target)
          else
            (grid, robot)

        case _ =>
          (grid, robot)

  private def moveEntity(grid: Grid, from: Pos, to: Pos): Grid =
    val entity = cellAt(grid, from)
    updateCell(updateCell(grid, from, '.'), to, entity)

  private def cellAt(grid: Grid, p: Pos): Char =
    grid(p.y)(p.x)

  private def updateCell(grid: Grid, pos: Pos, c: Char): Grid =
    val row = grid(pos.y)
    grid.updated(pos.y, row.updated(pos.x, c))

  private def isFloorLike(ch: Char): Boolean =
    !blockedChars.contains(ch)

  private def canPushAll(grid: Grid, start: Pos, move: Pos): (Boolean, Set[Pos]) =
    case class PushState(canPush: Boolean, seen: Set[Pos])

    def validateBoxPair(pos: Pos, c: Char): Option[Pos] =
      val otherX = if c == '[' then pos.x + 1 else pos.x - 1
      val otherPos = Pos(otherX, pos.y)
      val expected = if c == '[' then ']' else '['
      Option.when(inBounds(grid, otherPos) && cellAt(grid, otherPos) == expected)(otherPos)

    def evaluateCell(pos: Pos, seen: Set[Pos]): PushState =
      cellAt(grid, pos) match
        case '#' => PushState(false, seen)
        case c@('[' | ']') => evaluateBoxPair(pos, c, seen)
        case 'O' => evaluatePush(pos, seen)
        case _ => PushState(true, seen)

    def evaluateBoxPair(pos: Pos, matched: Char, seen: Set[Pos]): PushState =
      validateBoxPair(pos, matched) match
        case None => PushState(false, seen)
        case Some(otherPos) =>
          val (firstPos, secondPos) = if matched == '[' then (pos, otherPos) else (otherPos, pos)
          val PushState(ok1, s1) = evaluatePush(firstPos, seen)
          val PushState(ok2, s2) = evaluatePush(secondPos, s1)
          PushState(ok1 && ok2, s2)

    def evaluatePush(pos: Pos, seen: Set[Pos]): PushState =
      if seen.contains(pos) then PushState(true, seen)
      else
        val newPos = pos + move
        if !inBounds(grid, newPos) then PushState(false, seen)
        else evaluateCell(newPos, seen + pos)

    def evaluateStart(pos: Pos): (Boolean, Set[Pos]) =
      cellAt(grid, pos) match
        case c@('[' | ']') =>
          val PushState(ok, s) = evaluateBoxPair(pos, c, Set.empty)
          (ok, s)
        case 'O' =>
          val PushState(ok, s) = evaluatePush(pos, Set.empty)
          (ok, s)
        case _ => (false, Set.empty)

    evaluateStart(start)
  end canPushAll

  private def inBounds(grid: Grid, p: Pos): Boolean =
    p.y >= 0 && p.y < grid.size && p.x >= 0 && p.x < grid(p.y).size

  private def moveBoxes(grid: Grid, boxes: Set[Pos], move: Pos): Grid =
    val sortedBoxes = boxes.toSeq.sortBy: b =>
      if move.x > 0 then -b.x
      else if move.x < 0 then b.x
      else if move.y > 0 then -b.y
      else b.y

    sortedBoxes.foldLeft(grid): (g, b) =>
      val c = cellAt(g, b)
      val nb = b + move
      updateCell(updateCell(g, b, '.'), nb, c)

  private def sumBoxGPS(grid: Grid, checkChar: Char): Int =
    val boxGPS = for
      (row, y) <- grid.zipWithIndex
      (c, x) <- row.zipWithIndex
      if c == checkChar
    yield 100 * y + x

    boxGPS.sum

  private def scaleGrid(grid: Grid): Grid =
    grid.map: row =>
      row.flatMap:
        case '#' => "##"
        case 'O' => "[]"
        case '.' => ".."
        case '@' => "@."
        case c => s"$c$c"

  case class Pos(x: Int, y: Int):
    @targetName("addPos")
    def +(that: Pos): Pos = Pos(x + that.x, y + that.y)

end Day15
