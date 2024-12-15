package y2024

import scala.annotation.{tailrec, targetName}

class Day15 extends util.Day(15):
  type Grid = Vector[Vector[Char]]
  private val directionMap: Map[Char, Pos] = Map(
    '^' -> Pos(0, -1),
    'v' -> Pos(0, 1),
    '<' -> Pos(-1, 0),
    '>' -> Pos(1, 0))

  def solvePart1(input: IndexedSeq[String]): Int =
    solve(input, 'O')

  def solvePart2(input: IndexedSeq[String]): Int =
    solve(input, '[', scaleGrid)

  private def solve(input: IndexedSeq[String], matchChar: Char, tf: Grid => Grid = identity): Int =
    parseInput(input, tf).map:
      case (grid, robotPos, moves) =>
        runAllMoves(grid, robotPos, moves)
    .map(grid => sumBoxGPS(grid, matchChar)).getOrElse(sys.error("No solution"))

  def parseInput(input: IndexedSeq[String], tf: Grid => Grid = identity): Option[(Grid, Pos, Vector[Pos])] =
    val (gridLines, moveLines) = input.span(_.nonEmpty)
    val grid = tf(gridLines.map(_.toVector).toVector)
    findRobot(grid).map: robotPos =>
      val moves = moveLines.drop(1).mkString("").filter(directionMap.contains).map(directionMap).toVector
      (grid, robotPos, moves)

  private def findRobot(grid: Grid): Option[Pos] =
    for
      y <- grid.indices.find(grid(_).contains('@'))
      x <- grid(y).indices.find(grid(y)(_) == '@')
    yield Pos(x, y)

  private def runAllMoves(grid: Grid, start: Pos, moves: Vector[Pos]): Grid =
    @tailrec
    def recurse(state: (Grid, Pos), remainingMoves: Vector[Pos]): Grid =
      remainingMoves match
        case move +: rest =>
          val newState = attemptMove(state._1, state._2, move)
          recurse(newState, rest)
        case _ => state._1

    recurse((grid, start), moves)

  private def attemptMove(grid: Grid, robot: Pos, move: Pos): (Grid, Pos) =
    val target = robot + move
    if !inBounds(grid, target) || isWall(grid, target) then (grid, robot)
    else if isFloorLike(grid, target) then
      (moveEntity(grid, robot, target), target)
    else if isBoxCell(grid, target) then
      canPushAll(grid, target, move) match
        case (true, boxes) =>
          val newGrid = moveEntity(moveBoxes(grid, boxes, move), robot, target)
          (newGrid, target)
        case (false, _) => (grid, robot)
    else (grid, robot)

  private def moveEntity(grid: Grid, from: Pos, to: Pos): Grid =
    updateCell(updateCell(grid, from, '.'), to, grid(from.y)(from.x))

  private def canPushAll(grid: Grid, start: Pos, move: Pos): (Boolean, Set[Pos]) =
    case class PushState(canPush: Boolean, seen: Set[Pos])

    def validateBoxPair(pos: Pos, matchedChar: Char): Option[Pos] =
      val otherPos = Pos(pos.x + (if matchedChar == '[' then 1 else -1), pos.y)
      val expectedChar = if matchedChar == '[' then ']' else '['
      Option.when(inBounds(grid, otherPos) && grid(pos.y)(otherPos.x) == expectedChar)(otherPos)

    def evaluatePush(pos: Pos, seen: Set[Pos]): PushState =
      if seen.contains(pos) then
        PushState(true, seen)
      else
        val newPos = pos + move
        if !inBounds(grid, newPos) then
          PushState(false, seen)
        else
          evaluateCell(newPos, seen + pos)

    def evaluateCell(pos: Pos, seen: Set[Pos]): PushState =
      grid(pos.y)(pos.x) match
        case '#' => PushState(false, seen)
        case c@('[' | ']') => evaluateBoxPair(pos, c, seen)
        case 'O' => evaluatePush(pos, seen)
        case _ => PushState(true, seen)

    def evaluateBoxPair(pos: Pos, matched: Char, seen: Set[Pos]): PushState =
      validateBoxPair(pos, matched) match
        case None => PushState(false, seen)
        case Some(otherPos) =>
          val (firstPos, secondPos) = if matched == '[' then (pos, otherPos) else (otherPos, pos)
          val PushState(ok1, seen1) = evaluatePush(firstPos, seen)
          val PushState(ok2, seen2) = evaluatePush(secondPos, seen1)
          PushState(ok1 && ok2, seen2)

    def evaluateStart(pos: Pos): (Boolean, Set[Pos]) =
      grid(pos.y)(pos.x) match
        case c@('[' | ']') =>
          evaluateBoxPair(pos, c, Set.empty) match
            case PushState(ok, seen) => (ok, seen)
        case 'O' =>
          evaluatePush(pos, Set.empty) match
            case PushState(ok, seen) => (ok, seen)
        case _ =>
          (false, Set.empty)

    evaluateStart(start)
  end canPushAll

  private def moveBoxes(grid: Grid, boxes: Set[Pos], move: Pos): Grid =
    boxes.toSeq
      .sortBy(p => if move.x > 0 then -p.x else if move.x < 0 then p.x else if move.y > 0 then -p.y else p.y)
      .foldLeft(grid): (g, b) =>
        val c = g(b.y)(b.x)
        val nb = b + move
        updateCell(updateCell(g, b, '.'), nb, c)

  private def updateCell(grid: Grid, pos: Pos, c: Char): Grid =
    grid.updated(pos.y, grid(pos.y).updated(pos.x, c))

  private def inBounds(grid: Grid, p: Pos): Boolean =
    p.y >= 0 && p.y < grid.size && p.x >= 0 && p.x < grid(p.y).size

  private def isWall(grid: Grid, p: Pos): Boolean =
    grid(p.y)(p.x) == '#'

  private def isFloorLike(grid: Grid, p: Pos): Boolean =
    val ch = grid(p.y)(p.x)
    ch != '#' && ch != 'O' && ch != '[' && ch != ']'

  private def isBoxCell(grid: Grid, p: Pos): Boolean =
    val ch = grid(p.y)(p.x)
    ch == 'O' || ch == '[' || ch == ']'

  private def sumBoxGPS(grid: Grid, checkChar: Char): Int =
    grid.zipWithIndex.flatMap:
      case (row, y) =>
        row.zipWithIndex.collect:
          case (`checkChar`, x) => 100 * y + x
    .sum

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
