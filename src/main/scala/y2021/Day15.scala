package y2021

trait Day15 {
  case class Coord(x: Int, y: Int) extends Ordered[Coord] {
    def neighbors: Seq[Coord] = Seq(Coord(x - 1, y), Coord(x + 1, y), Coord(x, y - 1), Coord(x, y + 1))

    override def compare(that: Coord): Int = if y == that.y then x - that.x else y - that.y
  }

  case class Path(pt: Coord = Coord(0, 0), cost: Int = 0) extends Ordered[Path] {
    override def compare(that: Path): Int = cost - that.cost
  }

  object Puzzle {
    def apply(input: Seq[String]): Puzzle = {
      val map = (for y <- input.indices; x <- input(y).indices yield Coord(x, y) -> s"${input(y)(x)}".toInt).toMap
      Puzzle(map, Coord(input.head.length - 1, input.size - 1))
    }
  }

  case class Puzzle(map: Map[Coord, Int], goal: Coord) {
    lazy val solve: Int = {
      import scala.collection.mutable
      val queue = mutable.PriorityQueue[Path]().reverse
      val visited = mutable.Set[Coord]()
      queue.addOne(Path())
      while queue.nonEmpty && queue.head.pt != goal do {
        val head = queue.dequeue()
        head.pt.neighbors.filter(map.contains).filterNot(visited).map { pt =>
          Path(pt, head.cost + map(pt))
        }.foreach { path =>
          visited.add(path.pt)
          queue.addOne(path)
        }
      }
      queue.head.cost
    }

    lazy val expand: Puzzle = {
      def incr(score: Int, i: Int): Int = (((score - 1) + i) % 9) + 1

      val (maxX, maxY) = (map.keys.maxBy(_.x).x + 1, map.keys.maxBy(_.y).y + 1)
      val expandedMap = (0 until maxX).flatMap { x =>
        (0 until maxY).flatMap { y =>
          (0 until 5).flatMap { i =>
            (0 until 5).flatMap { j =>
              val (x2, y2, score) = (x + i * maxX, y + j * maxY, map(Coord(x, y)))
              Seq(Coord(x, y) -> score, Coord(x2, y) -> incr(score, i),
                Coord(x, y2) -> incr(score, j), Coord(x2, y2) -> incr(score, i + j))
            }
          }
        }
      }.toMap
      Puzzle(expandedMap, expandedMap.keys.max)
    }
  }
}
