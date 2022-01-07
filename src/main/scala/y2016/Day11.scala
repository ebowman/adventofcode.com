package y2016

trait Day11 {

  sealed trait Item extends Ordered[Item] {
    def name: String

    override def compare(that: Item): Int = {
      if (this.name.compare(that.name) == 0) {
        (this, that) match {
          case (Microchip(_), Generator(_)) => 1
          case _ => -1
        }
      } else this.name.compare(that.name)
    }
  }

  case class Generator(name: String) extends Item {
    override def toString = s"${name}G"
  }

  case class Microchip(name: String) extends Item {
    override def toString = s"${name}M"
  }

  def parseDescription(line: String): (Int, Set[Item]) = InputParser.parseAll(InputParser.floorDescription, line).get

  object InputParser extends scala.util.parsing.combinator.RegexParsers {

    override val whiteSpace: scala.util.matching.Regex = "".r

    def floorDescription: Parser[(Int, Set[Item])] = empty | nonEmpty | singleItem

    def singleItem: Parser[(Int, Set[Item])] = (floor <~ " contains ") ~ item <~ "." ^^ {
      case floor ~ item => floor -> Set(item)
    }

    def nonEmpty: Parser[(Int, Set[Item])] =
      (floor <~ " contains ") ~ repsep(item, ", ") ~ (",? and ".r ~> item <~ ".") ^^ {
        case floor ~ items ~ lastItem => floor -> (items.toSet + lastItem)
      }

    def empty: Parser[(Int, Set[Item])] = floor <~ " contains nothing relevant." ^^ (n => n -> Set())

    def item: Parser[Item] = microchip | generator

    def microchip: Parser[Item] = "a " ~> """\w+-compatible""".r <~ """ microchip""".r ^^
      (n => Microchip(s"${n.head.toUpper}${n.tail.head}"))

    def generator: Parser[Item] = "a " ~> """\w+""".r <~ """ generator""".r ^^
      (n => Generator(s"${n.head.toUpper}${n.tail.head}"))

    val floors = Map("first" -> 0, "second" -> 1, "third" -> 2, "fourth" -> 3)

    def floor: Parser[Int] = "The " ~> ("first" | "second" | "third" | "fourth") <~ " floor" ^^ (name => floors(name))
  }

  case class State(elevator: Int, floors: Map[Int, Set[Item]]) {
    lazy val simplify: (Int, Seq[(Int, Int)]) = {
      // prune the search space. Generator/chip pairs are fungible!
      // So this generates a distinct data structure that captures the pairings
      // but ignores the identities of the individual pairings. This
      // is what we track in the "visited" set in the dijkstra impl
      val tags = floors.flatMap(_._2).map(_.name).toSeq.distinct

      def distill(f: Item => Boolean): Map[String, Int] =
        (for (tag <- tags; (floor, items) <- floors; item <- items if f(item) && item.name == tag)
          yield tag -> floor).toMap

      val (generators, chips) = (distill(_.isInstanceOf[Generator]), distill(_.isInstanceOf[Microchip]))
      (elevator, tags.map(tag => (generators(tag), chips(tag))).sorted)
    }

    def nextStates: Seq[State] = {
      // prune the search space ... once above a certain level, never go back
      val minE = if (elevator == 1 && floors(0).isEmpty) 2
      else if (elevator == 2 && floors(0).isEmpty && floors(1).isEmpty) 3
      else math.max(elevator - 1, 0)
      for {
        nextFloor <- minE to math.min(elevator + 1, 3) if nextFloor != elevator
        items <- floors(elevator).toSeq.combinations(1) ++ floors(elevator).toSeq.combinations(2)
        nextState = copy(elevator = nextFloor, floors = floors +
          (elevator -> (floors(elevator) -- items)) + (nextFloor -> (floors(nextFloor) ++ items))) if nextState.isLegal
      } yield nextState
    }

    def isLegal: Boolean = floors.values.forall(isLegalFloor)

    def isLegalFloor(floor: Set[Item]): Boolean = if (floor.size < 2) true else {
      val microchips = floor.filter(_.isInstanceOf[Microchip]).map(_.name)
      val generators = floor.filter(_.isInstanceOf[Generator]).map(_.name)
      val solos = microchips -- generators
      if (solos.nonEmpty) generators.isEmpty else true
    }

    def isTarget: Boolean = (0 to 2).forall(f => floors(f).isEmpty)

    override def toString: String = {
      val ordering: Map[Item, Int] = floors.flatMap(_._2).toSeq.sorted.zipWithIndex.toMap
      val itemCount = ordering.values.max + 1
      val itemWidth = 4
      val canvas = {
        val tmp = Array.ofDim[Char](4, itemCount * itemWidth + 6)
        for (y <- tmp.indices; x <- tmp.head.indices) tmp(y)(x) = ' '
        tmp
      }
      for (floor <- 1 to 4) {
        canvas(4 - floor)(0) = 'F'
        canvas(4 - floor)(1) = s"$floor".head
        canvas(4 - floor)(3) = '.'
      }
      for (i <- 0 until ordering.size; j <- 0 until 4) {
        canvas(j)(6 + itemWidth * i) = '.'
      }
      canvas(3 - elevator)(3) = 'E'
      for {
        floor <- 0 until 4
        item <- floors(floor)
        i <- 0 until itemCount
      } {
        if (ordering(item) == i) {
          canvas(3 - floor)(6 + itemWidth * i) = item.toString.head
          canvas(3 - floor)(6 + itemWidth * i + 1) = item.toString.tail.head
          canvas(3 - floor)(6 + itemWidth * i + 2) = item.toString.tail.tail.head
        }
      }
      "\n" + "-" * (5 + itemWidth * ordering.size) + "\n" + canvas.map(_.mkString).mkString("\n") +
        "\n" + "-" * (5 + itemWidth * ordering.size)
    }
  }

  case class StatePath(states: List[State]) extends Ordered[StatePath] {
    override def compare(that: StatePath): Int = this.states.size - that.states.size

    def nextPaths: Seq[StatePath] = states.head.nextStates.map(state => copy(states = state :: states))

    def dijkstra: StatePath = {
      val queue = collection.mutable.PriorityQueue[StatePath]().reverse
      val visited = collection.mutable.Set[(Int, Seq[(Int, Int)])]()
      queue.addOne(this)
      while (queue.nonEmpty && !queue.head.states.head.isTarget) {
        val path = queue.dequeue()
        path.nextPaths.filterNot(p => visited(p.states.head.simplify)).foreach { path =>
          queue.addOne(path)
          visited.add(path.states.head.simplify)
        }
      }
      queue.head
    }
  }

  def solve(input: Seq[String], extra: Seq[Item] = Seq.empty): Int = {
    val initialState = input.map(parseDescription).toMap
    val bestPath = StatePath(List(State(0, initialState + (0 -> (initialState(0) ++ extra))))).dijkstra
    println(bestPath.states.reverse)
    bestPath.states.size - 1
  }
}
