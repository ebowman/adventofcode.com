package y2022

import scala.annotation.tailrec
import scala.collection.mutable

trait Day24:
  enum Direction:
    case Up, Down, Left, Right

  case class Pos(row: Int, col: Int)
  case class Blizzard(pos: Pos, dir: Direction)
  case class Valley(walls: Set[Pos], start: Pos, end: Pos, maxRow: Int, maxCol: Int)
  case class BlizzardCache(states: Array[Set[Pos]], cycleLength: Int)

  def parseInput(input: Seq[String]): (Valley, Set[Blizzard]) =
    val height = input.size
    val width = input.head.length

    val walls = (for
      row <- input.indices
      col <- input.head.indices
      if input(row)(col) == '#'
    yield Pos(row, col)).toSet

    val blizzards = (for
      row <- input.indices
      col <- input.head.indices
      char = input(row)(col)
      dir <- char match
        case '^' => Some(Direction.Up)
        case 'v' => Some(Direction.Down)
        case '<' => Some(Direction.Left)
        case '>' => Some(Direction.Right)
        case _ => None
    yield Blizzard(Pos(row, col), dir)).toSet

    val start = Pos(0, input.head.indexOf('.'))
    val end = Pos(height - 1, input(height - 1).indexOf('.'))

    (Valley(walls, start, end, height, width), blizzards)

  private def moveBlizzard(blizzard: Blizzard, maxRow: Int, maxCol: Int): Blizzard =
    val newPos = blizzard.dir match
      case Direction.Up =>
        val newRow = if blizzard.pos.row - 1 <= 0 then maxRow - 2 else blizzard.pos.row - 1
        Pos(newRow, blizzard.pos.col)
      case Direction.Down =>
        val newRow = if blizzard.pos.row + 1 >= maxRow - 1 then 1 else blizzard.pos.row + 1
        Pos(newRow, blizzard.pos.col)
      case Direction.Left =>
        val newCol = if blizzard.pos.col - 1 <= 0 then maxCol - 2 else blizzard.pos.col - 1
        Pos(blizzard.pos.row, newCol)
      case Direction.Right =>
        val newCol = if blizzard.pos.col + 1 >= maxCol - 1 then 1 else blizzard.pos.col + 1
        Pos(blizzard.pos.row, newCol)
    Blizzard(newPos, blizzard.dir)

  private def isValidMove(pos: Pos, valley: Valley): Boolean =
    (pos.row >= 0 && pos.row < valley.maxRow &&
      pos.col >= 0 && pos.col < valley.maxCol &&
      !valley.walls(pos)) || pos == valley.start || pos == valley.end

  private def calculateBlizzardCache(valley: Valley, initialBlizzards: Set[Blizzard]): BlizzardCache =
    val cycleLength = lcm(valley.maxRow - 2, valley.maxCol - 2)
    val states = Array.ofDim[Set[Pos]](cycleLength)
    var currentBlizzards = initialBlizzards

    for minute <- 0 until cycleLength do
      states(minute) = currentBlizzards.map(_.pos)
      currentBlizzards = currentBlizzards.map(moveBlizzard(_, valley.maxRow, valley.maxCol))

    BlizzardCache(states, cycleLength)

  private def findPath(from: Pos, to: Pos, startTime: Int, valley: Valley, blizzardCache: BlizzardCache): Int =
    case class State(pos: Pos, time: Int)
    val visited = mutable.HashSet[(Pos, Int)]()
    val queue = mutable.Queue[State]()
    queue.enqueue(State(from, startTime))

    while queue.nonEmpty do
      val State(pos, time) = queue.dequeue()
      if pos == to then return time

      val stateKey = (pos, time % blizzardCache.cycleLength)
      if !visited(stateKey) then
        visited.add(stateKey)
        val nextBlizzards = blizzardCache.states((time + 1) % blizzardCache.cycleLength)

        val moves = mutable.ArrayBuffer[Pos]()
        // Prioritize moves based on goal direction
        if from.row < to.row then
          moves ++= Seq(
            Pos(pos.row + 1, pos.col),
            Pos(pos.row, pos.col + 1),
            Pos(pos.row, pos.col - 1),
            Pos(pos.row - 1, pos.col)
          )
        else
          moves ++= Seq(
            Pos(pos.row - 1, pos.col),
            Pos(pos.row, pos.col + 1),
            Pos(pos.row, pos.col - 1),
            Pos(pos.row + 1, pos.col)
          )
        moves += pos // wait

        moves.foreach: nextPos =>
          if isValidMove(nextPos, valley) && !nextBlizzards(nextPos) then
            queue.enqueue(State(nextPos, time + 1))

    Int.MaxValue

  private def lcm(a: Int, b: Int): Int =
    @tailrec
    def gcd(x: Int, y: Int): Int = if y == 0 then x else gcd(y, x % y)
    (a * b) / gcd(a, b)

  def solve(input: Seq[String], isPart2: Boolean): Int =
    val (valley, initialBlizzards) = parseInput(input)
    val blizzardCache = calculateBlizzardCache(valley, initialBlizzards)

    val firstTrip = findPath(valley.start, valley.end, 0, valley, blizzardCache)
    if !isPart2 then return firstTrip

    val secondTrip = findPath(valley.end, valley.start, firstTrip, valley, blizzardCache)
    val thirdTrip = findPath(valley.start, valley.end, secondTrip, valley, blizzardCache)
    thirdTrip

  def solvePart1(input: Seq[String]): Int = solve(input, isPart2 = false)
  def solvePart2(input: Seq[String]): Int = solve(input, isPart2 = true)

end Day24
