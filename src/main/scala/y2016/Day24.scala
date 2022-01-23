package y2016

import scala.util.Try

trait Day24 {
  type Coord = (Int, Int)

  def grid: Seq[String]

  case class Path(head: Coord, size: Int) extends Comparable[Path] {
    override def compareTo(o: Path): Int = size - o.size

    def next: Seq[Path] = Seq((-1, 0), (1, 0), (0, -1), (0, 1)).map(d => (head._1 + d._1, head._2 + d._2)).collect {
      case p if !Try(grid(p._1)(p._2)).toOption.contains('#') => copy(head = p, size + 1)
    }
  }

  lazy val digitToCoord: Map[Char, (Int, Int)] =
    (for y <- grid.indices; x <- grid.head.indices if grid(y)(x).isDigit yield grid(y)(x) -> (y, x)).toMap

  lazy val segmentToPath: Map[(Char, Char), Path] =
    digitToCoord.keys.toSeq.combinations(2).map(x => (x.head, x(1))).flatMap { (a, b) =>
      val path = dijkstra(digitToCoord(a), digitToCoord(b))
      Seq((a, b) -> path, (b, a) -> path)
    }.toMap

  def combinedLen(paths: Seq[Path]): Int = paths.map(_.size).sum

  def solve1: Int =
    digitToCoord.keys.toSeq.permutations.withFilter(_.head == '0').map { perm =>
      combinedLen(perm.zip(perm.tail).map(segmentToPath))
    }.min

  def solve2: Int =
    digitToCoord.keys.toSeq.permutations.withFilter(_.head == '0').map(perm => perm :+ '0').map { perm =>
      combinedLen(perm.zip(perm.tail).map(segmentToPath))
    }.min

  def dijkstra(start: Coord, goal: Coord): Path = {
    import collection.mutable
    val queue = mutable.PriorityQueue[Path]().reverse
    val seen = mutable.Set[Coord]()
    queue.addOne(Path(start, 0))
    while (queue.nonEmpty && queue.head.head != goal) {
      val path = queue.dequeue()
      path.next.filterNot(p => seen(p.head)).foreach { nextPath =>
        seen += nextPath.head
        queue.addOne(nextPath)
      }
    }
    queue.head
  }
}
