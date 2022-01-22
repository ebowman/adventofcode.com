package y2016

import collection.mutable
import java.security.MessageDigest

trait Day17 {
  val messageDigest: MessageDigest = MessageDigest.getInstance("MD5")

  object Path {
    val deltaToDir = Map((-1, 0) -> 'L', (1, 0) -> 'R', (0, -1) -> 'U', (0, 1) -> 'D')
    val dirToIndex = Map('U' -> 0, 'D' -> 1, 'L' -> 2, 'R' -> 3)

    implicit class PtOps(x: (Int, Int)) {
      def +(y: (Int, Int)): (Int, Int) = (x._1 + y._1, x._2 + y._2)
    }

    def toHash(str: String): String = {
      import at.favre.lib.bytes.Bytes
      def toHex(buf: Array[Byte]): String = Bytes.from(buf).encodeHex(false).take(4)

      toHex(messageDigest.digest(str.getBytes))
    }

    def bfs(hash: String): Path = {
      val queue = mutable.PriorityQueue[Path](Path(List((0, 0)), hash)).reverse
      while !queue.head.isGoal do queue.dequeue().next.foreach(queue.addOne)
      queue.head
    }

    def dfs(max: Int = Int.MinValue)(path: Path): Int =
      if path.isGoal then math.max(max, path.steps.size - 1)
      else path.next.map(dfs(max)).maxOption.getOrElse(Int.MinValue)

    def apply(hash: String): Path = Path((0, 0) :: Nil, hash)
  }

  sealed case class Path(steps: List[(Int, Int)], hash: String) extends Ordered[Path] {

    import Path._

    override def compare(that: Path): Int = this.steps.size - that.steps.size

    def isGoal: Boolean = steps.head == (3, 3)

    def isLegal(pt: (Int, Int)): Boolean = pt._1 >= 0 && pt._1 < 4 && pt._2 >= 0 && pt._2 < 4

    def isOpen(hash: String, delta: (Int, Int)): Boolean = hash(dirToIndex(deltaToDir(delta))) >= 'b'

    val hashed: String = toHash(hash)

    def next: Seq[Path] =
      for delta <- Seq((-1, 0), (1, 0), (0, -1), (0, 1))
           if isLegal(steps.head + delta) && isOpen(hashed, delta) yield
        Path((steps.head + delta) :: steps, hash + deltaToDir(delta))
  }

  def solve1(input: String): String = Path.bfs(input).hash.drop(input.length)

  def solve2(input: String): Int = Path.dfs()(Path(input))
}