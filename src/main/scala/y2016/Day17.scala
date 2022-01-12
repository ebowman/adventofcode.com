package y2016

import collection.mutable
import java.security.MessageDigest

trait Day17 {
  val messageDigest: MessageDigest = MessageDigest.getInstance("MD5")

  object Path {
    val d2l = Map((-1, 0) -> 'L', (1, 0) -> 'R', (0, -1) -> 'U', (0, 1) -> 'D')
    val l2i = Map('U' -> 0, 'D' -> 1, 'L' -> 2, 'R' -> 3)

    implicit class PtOps(x: (Int, Int)) {
      def +(y: (Int, Int)): (Int, Int) = (x._1 + y._1, x._2 + y._2)
    }

    def toHash(str: String): String = {
      import at.favre.lib.bytes.Bytes
      def toHex(buf: Array[Byte]): String = Bytes.from(buf).encodeHex(false).take(4)

      toHex(messageDigest.digest(str.getBytes))
    }

    def bfs(hash: String): Path = {
      val queue = mutable.PriorityQueue[Path](Path(List((0, 0)), hash))(
        (x: Path, y: Path) => x.steps.size - y.steps.size).reverse
      while (!queue.head.isGoal) queue.dequeue().next.foreach(queue.addOne)
      queue.head
    }

    def dfs(hash: String): Int = {
      var max: Int = Int.MinValue

      def recurse(path: Path): Unit =
        if (path.isGoal) max = math.max(max, path.steps.size)
        else path.next.foreach(recurse)

      recurse(Path((0, 0) :: Nil, hash))
      max
    }
  }

  case class Path(steps: List[(Int, Int)], hash: String) {

    import Path._

    def isGoal: Boolean = steps.head == (3, 3)

    def isLegal(pt: (Int, Int)): Boolean = pt._1 >= 0 && pt._1 < 4 && pt._2 >= 0 && pt._2 < 4

    def isOpen(hash: String, i: Int): Boolean = hash(i) >= 'b'

    val hashed: String = toHash(hash)

    def next: Seq[Path] =
      for (delta <- Seq((-1, 0), (1, 0), (0, -1), (0, 1))
           if isLegal(steps.head + delta) && isOpen(hashed, l2i(d2l(delta)))) yield
        Path((steps.head + delta) :: steps, hash + d2l(delta))
  }

  def solve1(input: String): String = Path.bfs(input).hash.drop(input.length)

  def solve2(input: String): Int = Path.dfs(input) - 1
}