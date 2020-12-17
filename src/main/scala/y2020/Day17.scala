package y2020


trait Day17 {

  val ACTIVE = '#'
  val INACTIVE = '.'

  import collection.mutable

  type P3 = (Int, Int, Int)
  type P4 = (Int, Int, Int, Int)

  trait Ops[T] {
    def neighbors(pt: T): Iterator[T]

    def mkBoard(seed: IndexedSeq[String]): Set[T]

    def nextBoard(): mutable.HashSet[T]

    def neighborCount(): mutable.HashMap[T, Int]
  }

  def solve[T](input: IndexedSeq[String], count: Int = 6, ops: Ops[T]): Int = {
    var board = ops.mkBoard(input)
    for (_ <- 0 until count) {
      val nextBoard = ops.nextBoard()
      val neighborCount = ops.neighborCount()
      for (pt <- board; neighbor <- ops.neighbors(pt)) neighborCount(neighbor) += 1
      for ((pt, count) <- neighborCount)
        if ((board.contains(pt) && count == 2 || count == 3) || (!board.contains(pt) && count == 3)) nextBoard.add(pt)
      board = nextBoard.toSet
    }
    board.size
  }

  object Ops3 extends Ops[P3] {

    implicit class Rich(x: P3) {
      def +(y: P3): P3 = (x._1 + y._1, x._2 + y._2, x._3 + y._3)
    }

    def neighbors(pt: P3): Iterator[P3] = {
      val deltaIter = (for (x <- -1 to 1; y <- -1 to 1; z <- -1 to 1
                            if !(x == 0 && y == 0 && z == 0)) yield (x, y, z)).toIterator
      deltaIter.map(_ + pt)
    }

    def mkBoard(seed: IndexedSeq[String]): Set[P3] = {
      (for {
        y <- seed.indices
        x <- seed(y).indices if seed(y).charAt(x) == ACTIVE
      } yield (x, y, 0)).toSet
    }

    def nextBoard(): mutable.HashSet[P3] = new mutable.HashSet[P3]

    def neighborCount(): mutable.HashMap[P3, Int] =
      new mutable.HashMap[P3, Int] {
        override def default(key: P3): Int = 0
      }
  }

  def part1(input: IndexedSeq[String], count: Int = 6): Int = {
    solve(input, count, Ops3)
  }

  object Ops4 extends Ops[P4] {

    implicit class Rich(x: P4) {
      def +(y: P4): P4 = (x._1 + y._1, x._2 + y._2, x._3 + y._3, x._4 + y._4)
    }

    def neighbors(pt: P4): Iterator[P4] = {
      val deltaIter = (for (x <- -1 to 1; y <- -1 to 1; z <- -1 to 1; w <- -1 to 1
                            if !(x == 0 && y == 0 && z == 0 && w == 0)) yield (x, y, z, w)).toIterator
      deltaIter.map(_ + pt)
    }

    def mkBoard(seed: IndexedSeq[String]): Set[P4] = {
      (for {
        y <- seed.indices
        x <- seed(y).indices if seed(y).charAt(x) == ACTIVE
      } yield (x, y, 0, 0)).toSet
    }

    def nextBoard() = new mutable.HashSet[P4]

    def neighborCount(): mutable.HashMap[(Int, Int, Int, Int), Int] = new mutable.HashMap[P4, Int] {
      override def default(key: P4): Int = 0
    }
  }

  def part2(input: IndexedSeq[String], count: Int = 6): Int = {
    solve(input, count, Ops4)
  }
}


