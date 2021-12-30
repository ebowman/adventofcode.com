package y2021

import scala.annotation.tailrec

trait Day23 {

  type Point = (Int, Int)

  def coords: Seq[Point]

  def neighbors: Point => Seq[Point]

  def finalState: Config

  case class Path(states: List[Config] = Nil) extends Ordered[Path] {
    val cost: Int = states.map(_.cost).sum
    val endState: Config = states.head

    def nextMove: Seq[Path] = for {
      ampCoord <- coords if endState.at(ampCoord) != '.'
      move <- endState.nextMoves(ampCoord) if move.size > 1 && endState.isLegalPath(move)
      newState = endState.moveAmp(move.last, move.head) if !states.contains(newState)
      cost = Config.energy(endState.at(ampCoord)) * (move.size - 1)
    } yield copy(states = Config(state = newState, cost = cost) :: states)

    override def compare(that: Path): Int = this.cost - that.cost

    override def toString: String = s"Path(${states.reverse})"
  }

  object Config {
    val energy = Map('A' -> 1, 'B' -> 10, 'C' -> 100, 'D' -> 1000)
    val illegalSpots = Set((0, 2), (0, 4), (0, 6), (0, 8))

    def part1(input: Seq[String]): Config = {
      val letters = input.mkString.filter(_.isLetter)
      Config.init(
        room1 = IndexedSeq(letters(0), letters(4)),
        room2 = IndexedSeq(letters(1), letters(5)),
        room3 = IndexedSeq(letters(2), letters(6)),
        room4 = IndexedSeq(letters(3), letters(7)))
    }

    def part2(input: Seq[String]): Config = {
      val letters = input.mkString.filter(_.isLetter)
      Config.init(
        room1 = IndexedSeq(letters(0), 'D', 'D', letters(4)),
        room2 = IndexedSeq(letters(1), 'C', 'B', letters(5)),
        room3 = IndexedSeq(letters(2), 'B', 'A', letters(6)),
        room4 = IndexedSeq(letters(3), 'A', 'C', letters(7)))
    }

    def init(hallway: IndexedSeq[Char] = ("." * 11).toIndexedSeq,
             room1: IndexedSeq[Char],
             room2: IndexedSeq[Char],
             room3: IndexedSeq[Char],
             room4: IndexedSeq[Char]): Config =
      new Config(IndexedSeq(hallway, room1, room2, room3, room4), 0)
  }

  case class Config(state: IndexedSeq[IndexedSeq[Char]], cost: Int) {
    val rooms: IndexedSeq[IndexedSeq[Char]] = state

    private def matches(that: Config): Boolean = this.state == that.state

    def dijkstra(print: Boolean): Int = {
      import collection.mutable
      val queue = mutable.PriorityQueue[Path]().reverse
      val visited = mutable.Set[Config]()
      queue.addOne(Path(List(this)))
      while (queue.nonEmpty && !queue.head.endState.matches(finalState)) {
        val path = queue.dequeue()
        path.nextMove.filter(p => p.endState.matches(finalState) || !visited(p.endState)).foreach { path =>
          queue.addOne(path)
          visited.add((path.endState))
        }
      }
      if (print) println(queue.head)
      queue.head.cost
    }

    def at(c: Point): Char = state(c._1)(c._2)

    def nextMoves(from: Point): Seq[Seq[Point]] = {
      def nextMovesLong(p: List[Point]): Seq[List[Point]] =
        neighbors(p.head).filter(c => at(c) == '.').filterNot(p.contains) match {
          case seq if seq.isEmpty => List(p)
          case neighbors => neighbors.map(_ :: p).flatMap(nextMovesLong)
        }

      @tailrec def subMoves(m: List[Point], accum: List[List[Point]] = Nil): List[List[Point]] =
        if (m.size == 1) accum
        else subMoves(m.tail, m :: accum)

      nextMovesLong(List(from)).filter(_.size > 1).flatMap(m => subMoves(m))
    }

    def moveAmp(from: Point, to: Point): IndexedSeq[IndexedSeq[Char]] =
      state.updated(from._1, state(from._1).updated(from._2, '.')).
        updated(to._1, state(to._1).updated(to._2, at(from)))

    def isLegalPath(path: Seq[Point]): Boolean = {
      val amp = at(path.last)
      val homeRoom = amp - 'A' + 1
      val maxRoomIdx = coords.filter(_._1 == 1).maxBy(_._2)._2
      (path.last, path.head) match {
        // rule 3: no hallway-to-hallway moves
        case ((0, _), (0, _)) => false
        // rule 1: can't stop immediately outside a room
        case ((_, _), dest@(0, _)) if Config.illegalSpots.contains(dest) => false
        // rule 2: moving from hallway to room (or room to room)
        case ((_, _), (room, _)) if room >= 1 && room <= 4 && (room != homeRoom ||
          !rooms(room).forall(i => i == '.' || i == amp)) => false
        // my heuristic: never move from bottom of the room room to anywhere
        case ((room, idx), (_, _)) if room == homeRoom &&
          (idx to maxRoomIdx).forall(i => rooms(room)(i) == amp) => false
        // my heuristic: if dest is homeRoom(0) & homeRoom(1) is empty, not allowed
        case ((_, _), (room, i)) if room == homeRoom && i < maxRoomIdx && rooms(room)(i + 1) == '.' => false
        // my heuristic: nobody ever moves from 1 to 0 in a room
        // (homeRoom, 1) is the only option
        case ((room, i), (room2, j)) if room == room2 && i <= maxRoomIdx && j < i => false
        case _ => true
      }
    }

    override def toString: String = {
      def hallway: IndexedSeq[Char] = state(0)

      if (rooms(1).size == 2) {
        s"\n${"#" * 13}\n" +
          s"#${hallway.mkString}#\n" +
          s"###${rooms(1)(0)}#${rooms(2)(0)}#${rooms(3)(0)}#${rooms(4)(0)}### ${cost}\n" +
          s"  #${rooms(1)(1)}#${rooms(2)(1)}#${rooms(3)(1)}#${rooms(4)(1)}#  \n" +
          s"  #########  \n"
      } else {
        s"\n${"#" * 13}\n" +
          s"#${hallway.mkString}#\n" +
          s"###${rooms(1)(0)}#${rooms(2)(0)}#${rooms(3)(0)}#${rooms(4)(0)}### ${cost}\n" +
          s"  #${rooms(1)(1)}#${rooms(2)(1)}#${rooms(3)(1)}#${rooms(4)(1)}#  \n" +
          s"  #${rooms(1)(2)}#${rooms(2)(2)}#${rooms(3)(2)}#${rooms(4)(2)}#  \n" +
          s"  #${rooms(1)(3)}#${rooms(2)(3)}#${rooms(3)(3)}#${rooms(4)(3)}#  \n" +
          s"  #########  \n"
      }
    }
  }
}

trait Day23Part1 extends Day23 {

  val finalState: Config = Config.init(
    room1 = IndexedSeq('A', 'A'),
    room2 = IndexedSeq('B', 'B'),
    room3 = IndexedSeq('C', 'C'),
    room4 = IndexedSeq('D', 'D'))

  val coords: Seq[Point] = Seq(
    (0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (0, 7), (0, 8), (0, 9), (0, 10),
    (1, 0), (1, 1),
    (2, 0), (2, 1),
    (3, 0), (3, 1),
    (4, 0), (4, 1))

  val neighbors = Map[Point, Seq[Point]](
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

  val finalState: Config = Config.init(
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

  val neighbors = Map[Point, Seq[Point]](
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
