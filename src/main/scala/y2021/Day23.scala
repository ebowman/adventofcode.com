package y2021

import scala.annotation.tailrec

trait Day23 {

  def coords: Seq[(Int, Int)]

  def neighbors(coord: (Int, Int)): List[(Int, Int)]

  def finalState: Config

  object Config {
    def init(hallway: IndexedSeq[Char] = ("." * 11).toIndexedSeq,
              room1: IndexedSeq[Char],
              room2: IndexedSeq[Char],
              room3: IndexedSeq[Char],
              room4: IndexedSeq[Char]): Config =
      new Config(IndexedSeq(hallway, room1, room2, room3, room4), 0)

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

    val energy = Map('A' -> 1, 'B' -> 10, 'C' -> 100, 'D' -> 1000)
    val illegalSpots = Set((0, 2), (0, 4), (0, 6), (0, 8))

    case class Path(states: List[Config] = Nil) extends Ordered[Path] {
      override def compare(that: Path): Int = this.cost - that.cost

      def cost: Int = states.map(_.cost).sum

      override def toString: String = s"Path(${states.reverse})"

      def endState: Config = states.head

      def nextMove: Seq[Path] = (for {
        ampCoord <- coords if endState.at(ampCoord) != '.'
        move <- endState.allNextMoves(ampCoord) if move.size > 1
        subMove <- subMoves(move) if subMove.size > 1 && isLegalPath(endState, subMove)
        newState = endState.moveAmp(subMove.last, subMove.head) if !states.contains(newState)
        cost = energy(endState.at(ampCoord)) * (subMove.size - 1)
      } yield copy(states = Config(state = newState, cost = cost) :: states)).distinct
    }

    def subMoves(move: List[(Int, Int)]): List[List[(Int, Int)]] = {
      @tailrec def recurse(m: List[(Int, Int)], accum: List[List[(Int, Int)]] = Nil): List[List[(Int, Int)]] =
        if (m.size == 1) accum
        else recurse(m.tail, m :: accum)

      recurse(move)
    }

    def isLegalPath(config: Config, path: List[(Int, Int)]): Boolean = {
      val amp = config.at(path.last)
      val homeRoom = amp - 'A' + 1
      val maxRoomIdx = coords.filter(_._1 == 1).maxBy(_._2)._2
      (path.last, path.head) match {
        // rule 3: no hallway-to-hallway moves
        case ((0, _), (0, _)) => false
        // rule 1: can't stop immediately outside a room
        case ((_, _), dest@(0, _)) if illegalSpots.contains(dest) => false
        // rule 2: moving from hallway to room (or room to room)
        case ((_, _), (room, _)) if room >= 1 && room <= 4 && (room != homeRoom ||
          !config.rooms(room).forall(i => i == '.' || i == amp)) => false
        // my heuristic: never move from bottom of the room room to anywhere
        case ((room, idx), (_, _)) if room == homeRoom && idx == maxRoomIdx => false
        // my heuristic: if dest is homeRoom(0) & homeRoom(1) is empty, not allowed
        case ((_, _), (room, i)) if room == homeRoom && i < maxRoomIdx && config.rooms(room)(i + 1) == '.' => false
        // my heuristic: nobody ever moves from 1 to 0 in a room
        // (homeRoom, 1) is the only option
        case ((room, i), (room2, j)) if room == room2 && i <= maxRoomIdx && j < i => false
        case _ => true
      }
    }
  }

  case class Config(state: IndexedSeq[IndexedSeq[Char]], cost: Int) {

    import Config.Path

    def rooms: IndexedSeq[IndexedSeq[Char]] = state

    def at(c: (Int, Int)): Char = state(c._1)(c._2)

    def matches(that: Config): Boolean = this.state == that.state

    override def toString: String = {
      def hallway: IndexedSeq[Char] = state(0)

      if (rooms(1).size == 2) {
        s"\n${"#" * 13}\n" +
          s"#${hallway.mkString}#\n" +
          s"###${rooms(1)(0)}#${rooms(2)(0)}#${rooms(3)(0)}#${rooms(4)(0)}###\n" +
          s"  #${rooms(1)(1)}#${rooms(2)(1)}#${rooms(3)(1)}#${rooms(4)(1)}#  \n" +
          s"  #########  \n"
      } else {
        s"\n${"#" * 13}\n" +
          s"#${hallway.mkString}#\n" +
          s"###${rooms(1)(0)}#${rooms(2)(0)}#${rooms(3)(0)}#${rooms(4)(0)}###\n" +
          s"  #${rooms(1)(1)}#${rooms(2)(1)}#${rooms(3)(1)}#${rooms(4)(1)}#  \n" +
          s"  #${rooms(1)(2)}#${rooms(2)(2)}#${rooms(3)(2)}#${rooms(4)(2)}#  \n" +
          s"  #${rooms(1)(3)}#${rooms(2)(3)}#${rooms(3)(3)}#${rooms(4)(3)}#  \n" +
          s"  #########  \n"
      }
    }

    def allNextMoves(path: (Int, Int)): List[List[(Int, Int)]] = {
      def recurse(p: List[(Int, Int)]): List[List[(Int, Int)]] =
        neighbors(p.head).filter(c => this.at(c) == '.').filterNot(p.contains) match {
          case seq if seq.isEmpty => List(p)
          case neighbors => neighbors.map(_ :: p).flatMap(recurse)
        }

      recurse(List(path))
    }

    def moveAmp(from: (Int, Int), to: (Int, Int)): IndexedSeq[IndexedSeq[Char]] =
      state.updated(from._1, state(from._1).updated(from._2, '.')).
        updated(to._1, state(to._1).updated(to._2, at(from)))

    def findSolution(print: Boolean): Int = {
      import collection.mutable
      val queue = mutable.PriorityQueue[Path]().reverse
      val visited = mutable.Set[(Config)]()
      queue.addOne(Path(List(this)))
      while (queue.nonEmpty && !queue.head.endState.matches(finalState)) {
        val path = queue.dequeue()
        path.nextMove.filter(p => p.endState.matches(finalState) || !visited((p.endState))).foreach { path =>
          queue.addOne(path)
          visited.add((path.endState))
        }
      }
      if (print) println(queue.head)
      queue.head.cost
    }
  }
}

trait Day23Part1 extends Day23 {

  lazy val finalState: Config = Config.init(
    room1 = IndexedSeq('A', 'A'),
    room2 = IndexedSeq('B', 'B'),
    room3 = IndexedSeq('C', 'C'),
    room4 = IndexedSeq('D', 'D'))

  def coords: Seq[(Int, Int)] = Seq(
    (0, 0), (0, 1), (0, 2), (0, 3), (0, 4), (0, 5), (0, 6), (0, 7), (0, 8), (0, 9), (0, 10),
    (1, 0), (1, 1),
    (2, 0), (2, 1),
    (3, 0), (3, 1),
    (4, 0), (4, 1)
  )

  def neighbors(pt: (Int, Int)): List[(Int, Int)] = {
    pt._1 match {
      case 0 => pt._2 match {
        case 0 => List((0, 1))
        case 1 => List((0, 0), (0, 2))
        case 2 => List((0, 1), (0, 3), (1, 0))
        case 3 => List((0, 2), (0, 4))
        case 4 => List((0, 3), (0, 5), (2, 0))
        case 5 => List((0, 4), (0, 6))
        case 6 => List((0, 5), (0, 7), (3, 0))
        case 7 => List((0, 6), (0, 8))
        case 8 => List((0, 7), (0, 9), (4, 0))
        case 9 => List((0, 8), (0, 10))
        case 10 => List((0, 9))
      }
      case 1 => pt._2 match {
        case 0 => List((0, 2), (1, 1))
        case 1 => List((1, 0))
      }
      case 2 => pt._2 match {
        case 0 => List((0, 4), (2, 1))
        case 1 => List((2, 0))
      }
      case 3 => pt._2 match {
        case 0 => List((0, 6), (3, 1))
        case 1 => List((3, 0))
      }
      case 4 => pt._2 match {
        case 0 => List((0, 8), (4, 1))
        case 1 => List((4, 0))
      }
    }
  }
}

trait Day23Part2 extends Day23 {

  lazy val finalState: Config = Config.init(
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

  def neighbors(pt: (Int, Int)): List[(Int, Int)] = {
    pt._1 match {
      case 0 => pt._2 match {
        case 0 => List((0, 1))
        case 1 => List((0, 0), (0, 2))
        case 2 => List((0, 1), (0, 3), (1, 0))
        case 3 => List((0, 2), (0, 4))
        case 4 => List((0, 3), (0, 5), (2, 0))
        case 5 => List((0, 4), (0, 6))
        case 6 => List((0, 5), (0, 7), (3, 0))
        case 7 => List((0, 6), (0, 8))
        case 8 => List((0, 7), (0, 9), (4, 0))
        case 9 => List((0, 8), (0, 10))
        case 10 => List((0, 9))
      }
      case 1 => pt._2 match {
        case 0 => List((0, 2), (1, 1))
        case 1 => List((1, 0), (1, 2))
        case 2 => List((1, 1), (1, 3))
        case 3 => List((1, 2))
      }
      case 2 => pt._2 match {
        case 0 => List((0, 4), (2, 1))
        case 1 => List((2, 0), (2, 2))
        case 2 => List((2, 1), (2, 3))
        case 3 => List((2, 2))
      }
      case 3 => pt._2 match {
        case 0 => List((0, 6), (3, 1))
        case 1 => List((3, 0), (3, 2))
        case 2 => List((3, 1), (3, 3))
        case 3 => List((3, 2))
      }
      case 4 => pt._2 match {
        case 0 => List((0, 8), (4, 1))
        case 1 => List((4, 0), (4, 2))
        case 2 => List((4, 1), (4, 3))
        case 3 => List((4, 2))
      }
    }
  }
}
