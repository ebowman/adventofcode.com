package y2016

trait Day13 {
  case class memoize[A, B, C](f: ((A, B)) => C) extends (((A, B)) => C) {
    private val map = collection.mutable.Map[(A, B), C]()

    override def apply(x: (A, B)): C = map.getOrElseUpdate(x, f(x))
  }

  case class Board(magic: Int) {
    val open: ((Int, Int)) => Boolean = memoize((pt: (Int, Int)) => {
      val (x, y) = (pt._1, pt._2)
      val mask = x * x + 3 * x + 2 * x * y + y + y * y + magic
      (Integer.toBinaryString(mask).filter(_ == '1').sum % 2) == 0
    })

    def next(pt: (Int, Int)): Seq[(Int, Int)] = {
      val (x, y) = (pt._1, pt._2)
      Seq((x + 1, y), (x - 1, y), (x, y - 1), (x, y + 1)).filter(x => x._1 >= 0 && x._2 >= 0 && open(x))
    }

    def dijkstra(target: (Int, Int)): Option[List[(Int, Int)]] = {
      val queue = collection.mutable.PriorityQueue[List[(Int, Int)]]()(Ordering.by(_.size)).reverse
      val visited = collection.mutable.Set[(Int, Int)]()
      queue.addOne(List((1, 1)))
      while (queue.nonEmpty && queue.head.head != target) {
        val path = queue.dequeue()
        next(path.head).filterNot(visited).foreach { head =>
          queue.addOne(head :: path)
          visited.add(head)
        }
      }
      queue.headOption
    }
  }

  def solve1(magic: Int, goal: (Int, Int)): Int = Board(magic).dijkstra(goal).map(_.size - 1).getOrElse(0)

  def solve2(magic: Int): Int = {
    val board = Board(magic)
    (0 until 50).flatMap(y => (0 until 50).flatMap { x =>
      board.dijkstra((x, y)).collect {
        case path if path.size - 1 <= 50 => 1
        case _ => 0
      }
    }).sum
  }
}
