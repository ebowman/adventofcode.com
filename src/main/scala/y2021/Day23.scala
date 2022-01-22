package y2021

import scala.annotation.tailrec

trait Day23 {

  type Point = (Int, Int)

  def coords: Seq[Point]

  def neighbors: Point => Seq[Point]

  def load(input: Seq[String]): State

  def finalState: State

  case class StatePath(states: List[State] = Nil) extends Ordered[StatePath] {
    val cost: Int = states.map(_.cost).sum
    val latestConfig: State = states.head

    def nextMove: Seq[StatePath] = for
      nextAmpCoord <- coords if latestConfig.at(nextAmpCoord) != '.'
      nextPath <- latestConfig.nextPaths(nextAmpCoord) if latestConfig.isLegalPath(nextPath)
      nextState = latestConfig.generateNextConfig(nextPath) if !states.contains(nextState)
      cost = State.energy(latestConfig.at(nextAmpCoord)) * (nextPath.size - 1)
    yield copy(states = State(config = nextState, cost = cost) :: states)

    override def compare(that: StatePath): Int = this.cost - that.cost

    override def toString: String = s"Path(${states.reverse})"
  }

  case class State(config: IndexedSeq[IndexedSeq[Char]], cost: Int) {
    private def matches(that: State): Boolean = this.config == that.config

    def dijkstra(print: Boolean): Int = {
      import collection.mutable
      val queue = mutable.PriorityQueue[StatePath]().reverse
      val visited = mutable.Set[State]()
      queue.addOne(StatePath(List(this)))
      while queue.nonEmpty && !queue.head.latestConfig.matches(finalState) do {
        val path = queue.dequeue()
        path.nextMove.filterNot(p => visited(p.latestConfig)).foreach { path =>
          queue.addOne(path)
          visited.add(path.latestConfig)
        }
      }
      if print then println(queue.head)
      queue.head.cost
    }

    def at(c: Point): Char = config(c._1)(c._2)

    def nextPaths(from: Point): Seq[Seq[Point]] = {
      def longestAvailablePaths(path: List[Point]): Seq[List[Point]] =
        neighbors(path.head).filter(c => at(c) == '.').filterNot(path.contains) match {
          case seq if seq.isEmpty => List(path)
          case neighbors => neighbors.map(_ :: path).flatMap(longestAvailablePaths)
        }

      @tailrec def subPaths(accum: List[List[Point]] = Nil)(path: List[Point]): List[List[Point]] =
        if path.size == 1 then accum
        else subPaths(path :: accum)(path.tail)

      longestAvailablePaths(List(from)).flatMap(subPaths())
    }

    def generateNextConfig(path: Seq[Point]): IndexedSeq[IndexedSeq[Char]] = {
      val (from, to) = (path.last, path.head)
      config.updated(from._1, config(from._1).updated(from._2, '.')).
        updated(to._1, config(to._1).updated(to._2, at(from)))
    }

    def isLegalPath(path: Seq[Point]): Boolean = {
      val amp = at(path.last)
      val homeRoom = amp - 'A' + 1
      val maxRoomIdx = coords.filter(_._1 == 1).maxBy(_._2)._2
      (path.last, path.head) match {
        // rule 3: no hallway-to-hallway moves
        case ((0, _), (0, _)) => false
        // rule 1: can't stop immediately outside a room
        case ((_, _), dest@(0, _)) if State.illegalSpots.contains(dest) => false
        // rule 2: moving from hallway to room (or room to room)
        case ((_, _), (room, _)) if room >= 1 && room <= 4 && (room != homeRoom ||
          !config(room).forall(i => i == '.' || i == amp)) => false
        // my heuristic: never move from bottom of the room room to anywhere
        case ((room, idx), (_, _)) if room == homeRoom &&
          (idx to maxRoomIdx).forall(i => config(room)(i) == amp) => false
        // my heuristic: if dest is homeRoom(0) & homeRoom(1) is empty, not allowed
        case ((_, _), (room, i)) if room == homeRoom && i < maxRoomIdx && config(room)(i + 1) == '.' => false
        case _ => true
      }
    }

    override def toString: String = {
      def hallway: IndexedSeq[Char] = config(0)

      if config(1).size == 2 then {
        s"\n${"#" * 13}\n" +
          s"#${hallway.mkString}#\n" +
          s"###${config(1)(0)}#${config(2)(0)}#${config(3)(0)}#${config(4)(0)}### ${cost}\n" +
          s"  #${config(1)(1)}#${config(2)(1)}#${config(3)(1)}#${config(4)(1)}#  \n" +
          s"  #########  \n"
      } else {
        s"\n${"#" * 13}\n" +
          s"#${hallway.mkString}#\n" +
          s"###${config(1)(0)}#${config(2)(0)}#${config(3)(0)}#${config(4)(0)}### ${cost}\n" +
          s"  #${config(1)(1)}#${config(2)(1)}#${config(3)(1)}#${config(4)(1)}#  \n" +
          s"  #${config(1)(2)}#${config(2)(2)}#${config(3)(2)}#${config(4)(2)}#  \n" +
          s"  #${config(1)(3)}#${config(2)(3)}#${config(3)(3)}#${config(4)(3)}#  \n" +
          s"  #########  \n"
      }
    }
  }

  object State {
    val energy = Map('A' -> 1, 'B' -> 10, 'C' -> 100, 'D' -> 1000)
    val illegalSpots = Set((0, 2), (0, 4), (0, 6), (0, 8))

    def init(hallway: IndexedSeq[Char] = ("." * 11).toIndexedSeq,
             room1: IndexedSeq[Char],
             room2: IndexedSeq[Char],
             room3: IndexedSeq[Char],
             room4: IndexedSeq[Char]): State =
      new State(IndexedSeq(hallway, room1, room2, room3, room4), 0)
  }
}

trait Day23Part1 extends Day23 {

  val finalState: State = State.init(
    room1 = IndexedSeq('A', 'A'),
    room2 = IndexedSeq('B', 'B'),
    room3 = IndexedSeq('C', 'C'),
    room4 = IndexedSeq('D', 'D'))

  override def load(input: Seq[String]): State = {
    val letters = input.mkString.filter(_.isLetter)
    State.init(
      room1 = IndexedSeq(letters(0), letters(4)),
      room2 = IndexedSeq(letters(1), letters(5)),
      room3 = IndexedSeq(letters(2), letters(6)),
      room4 = IndexedSeq(letters(3), letters(7)))
  }

  val coords: Seq[Point] = Seq(
    (0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (0, 7), (0, 8), (0, 9), (0, 10),
    (1, 0), (1, 1),
    (2, 0), (2, 1),
    (3, 0), (3, 1),
    (4, 0), (4, 1))

  val neighbors: Point => Seq[Point] = Map[Point, Seq[Point]](
    (0, 0) -> Seq((0, 1)),
    (0, 1) -> Seq((0, 0), (0, 2)),
    (0, 2) -> Seq((0, 1), (0, 3), (1, 0)),
    (0, 3) -> Seq((0, 2), (0, 4)),
    (0, 4) -> Seq((0, 3), (0, 5), (2, 0)),
    (0, 5) -> Seq((0, 4), (0, 6)),
    (0, 6) -> Seq((0, 5), (0, 7), (3, 0)),
    (0, 7) -> Seq((0, 6), (0, 8)),
    (0, 8) -> Seq((0, 7), (0, 9), (4, 0)),
    (0, 9) -> Seq((0, 8), (0, 10)),
    (0, 10) -> Seq((0, 9)),
    (1, 0) -> Seq((0, 2), (1, 1)),
    (1, 1) -> Seq((1, 0)),
    (2, 0) -> Seq((0, 4), (2, 1)),
    (2, 1) -> Seq((2, 0)),
    (3, 0) -> Seq((0, 6), (3, 1)),
    (3, 1) -> Seq((3, 0)),
    (4, 0) -> Seq((0, 8), (4, 1)),
    (4, 1) -> Seq((4, 0))
  )
}

trait Day23Part2 extends Day23 {

  override def load(input: Seq[String]): State = {
    val letters = input.mkString.filter(_.isLetter)
    State.init(
      room1 = IndexedSeq(letters(0), 'D', 'D', letters(4)),
      room2 = IndexedSeq(letters(1), 'C', 'B', letters(5)),
      room3 = IndexedSeq(letters(2), 'B', 'A', letters(6)),
      room4 = IndexedSeq(letters(3), 'A', 'C', letters(7)))
  }

  val finalState: State = State.init(
    room1 = IndexedSeq('A', 'A', 'A', 'A'),
    room2 = IndexedSeq('B', 'B', 'B', 'B'),
    room3 = IndexedSeq('C', 'C', 'C', 'C'),
    room4 = IndexedSeq('D', 'D', 'D', 'D'))

  val coords = Seq(
    (0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (0, 7), (0, 8), (0, 9), (0, 10),
    (1, 0), (1, 1), (1, 2), (1, 3),
    (2, 0), (2, 1), (2, 2), (2, 3),
    (3, 0), (3, 1), (3, 2), (3, 3),
    (4, 0), (4, 1), (4, 2), (4, 3)
  )

  val neighbors: Point => Seq[Point] = Map[Point, Seq[Point]](
    (0, 0) -> Seq((0, 1)),
    (0, 1) -> Seq((0, 0), (0, 2)),
    (0, 2) -> Seq((0, 1), (0, 3), (1, 0)),
    (0, 3) -> Seq((0, 2), (0, 4)),
    (0, 4) -> Seq((0, 3), (0, 5), (2, 0)),
    (0, 5) -> Seq((0, 4), (0, 6)),
    (0, 6) -> Seq((0, 5), (0, 7), (3, 0)),
    (0, 7) -> Seq((0, 6), (0, 8)),
    (0, 8) -> Seq((0, 7), (0, 9), (4, 0)),
    (0, 9) -> Seq((0, 8), (0, 10)),
    (0, 10) -> Seq((0, 9)),
    (1, 0) -> Seq((0, 2), (1, 1)),
    (1, 1) -> Seq((1, 0), (1, 2)),
    (1, 2) -> Seq((1, 1), (1, 3)),
    (1, 3) -> Seq((1, 2)),
    (2, 0) -> Seq((0, 4), (2, 1)),
    (2, 1) -> Seq((2, 0), (2, 2)),
    (2, 2) -> Seq((2, 1), (2, 3)),
    (2, 3) -> Seq((2, 2)),
    (3, 0) -> Seq((0, 6), (3, 1)),
    (3, 1) -> Seq((3, 0), (3, 2)),
    (3, 2) -> Seq((3, 1), (3, 3)),
    (3, 3) -> Seq((3, 2)),
    (4, 0) -> Seq((0, 8), (4, 1)),
    (4, 1) -> Seq((4, 0), (4, 2)),
    (4, 2) -> Seq((4, 1), (4, 3)),
    (4, 3) -> Seq((4, 2)))
}
