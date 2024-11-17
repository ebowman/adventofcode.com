package y2023

import scala.collection.mutable

// see https://adventofcode.com/2023/day/17
trait Day17 {
  def solvePart1(input: Seq[String]): Int = solve(input, CrucibleParams.Regular)

  def solvePart2(input: Seq[String]): Int = solve(input, CrucibleParams.Ultra)

  private def solve(input: Seq[String], params: CrucibleParams): Int =
    // Parse and validate input
    val grid = parseInput(input)
    val (rows, cols) = (grid.length, grid(0).length)

    // Custom ordering for priority queue to minimize heat loss
    implicit val ordering: Ordering[QueueEntry] = Ordering.by[QueueEntry, Int](_.heatLoss).reverse

    val queue = mutable.PriorityQueue[QueueEntry]()
    val visited = mutable.Set[State]()

    // Initialize with starting positions
    val initialStates = List(State((0, 0), Direction.Right, 0), State((0, 0), Direction.Down, 0))
    initialStates.foreach(state => queue.enqueue(QueueEntry(state, 0)))

    def isValidMove(state: State, newDir: Direction): Boolean =
      val oppositeDir = Direction.fromIndex((state.dir.index + 2) % 4)
      newDir != oppositeDir && {
        if (state.straight == 0) true
        else if (state.straight < params.minStraight) newDir == state.dir
        else if (state.straight >= params.maxStraight) newDir != state.dir
        else true
      }

    def getNextStates(state: State): List[(State, Int)] = for {
        dirOffset <- List(-1, 0, 1)
        newDir = Direction.fromIndex((state.dir.index + dirOffset + 4) % 4) if isValidMove(state, newDir)
        nextStraight = if newDir == state.dir then state.straight + 1 else 1
        (dr, dc) = newDir.vector
        newRow = state.row + dr
        newCol = state.col + dc if newRow >= 0 && newRow < rows && newCol >= 0 && newCol < cols
      } yield (State((newRow, newCol), newDir, nextStraight), grid(newRow)(newCol))

    // Main pathfinding loop
    while queue.nonEmpty do {
      val QueueEntry(state, heatLoss) = queue.dequeue()
      if (isDestination(state, rows, cols, params.minStraight)) return heatLoss
      if (!visited(state)) then {
        visited.add(state)
        getNextStates(state).foreach { case (nextState, additionalHeat) =>
          queue.enqueue(QueueEntry(nextState, heatLoss + additionalHeat))
        }
      }
    }
    Int.MaxValue

  private def parseInput(input: Seq[String]): Array[Array[Int]] = input.map(_.trim.toCharArray.map(_.asDigit)).toArray

  private def isDestination(state: State, rows: Int, cols: Int, minStraight: Int): Boolean =
    state.row == rows - 1 && state.col == cols - 1 && state.straight >= minStraight

  sealed trait Direction:
    def vector: (Int, Int)
    def index: Int

  final case class CrucibleParams(minStraight: Int, maxStraight: Int)
  object CrucibleParams:
    val Regular = CrucibleParams(1, 3)
    val Ultra = CrucibleParams(4, 10)

  final case class State(pos: (Int, Int), dir: Direction, straight: Int):
    def row: Int = pos._1
    def col: Int = pos._2

  final case class QueueEntry(state: State, heatLoss: Int)

  object Direction:
    val all = Vector(Right, Down, Left, Up)
    def fromIndex(idx: Int): Direction = all(idx)
    case object Right extends Direction:
      def vector = (0, 1)
      def index = 0
    case object Down extends Direction:
      def vector = (1, 0)
      def index = 1
    case object Left extends Direction:
      def vector = (0, -1)
      def index = 2
    case object Up extends Direction:
      def vector = (-1, 0)
      def index = 3
}