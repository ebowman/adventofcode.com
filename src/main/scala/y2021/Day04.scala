package y2021

trait Day04 {
  case class Board(x: Array[Int] = Array.fill(100)(-1),
                   y: Array[Int] = Array.fill(100)(-1),
                   marks: Array[Boolean] = Array.fill(100)(false)) {
    def put(c: (Int, Int), v: Int): Unit = {
      x(v) = c._1
      y(v) = c._2
    }

    def coordOf(v: Int): (Int, Int) = (x(v), y(v))

    def isMarked(v: Int) = marks(v)

    def isMarked(c: (Int, Int)) = valOf(c).map(marks).getOrElse(false)

    def valOf(c: (Int, Int)): Option[Int] = (0 until 100).find(i => x(i) == c._1 && y(i) == c._2)

    def mark(m: Int): Unit = marks(m) = true

    def sumUnmarked: Int = (0 until 100).filter(i => x(i) != -1).filterNot(isMarked).sum

    def play(v: Int): Boolean = {
      if (coordOf(v) != (-1, -1)) mark(v)
      isBingo
    }

    def isBingo: Boolean = //noinspection DuplicatedCode
      ((0 until 5).exists(x => (0 until 5).map(y => (x, y)).count(isMarked) == 5)) ||
        (0 until 5).exists(y => (0 until 5).map(x => (x, y)).count(isMarked) == 5)
  }

  def loadBoard(lines: Iterable[String]): (Seq[Int], Set[Board]) = {
    val iter = lines.iterator
    val plays = iter.next().split(",").map(_.toInt).toSeq
    var set = Set[Board]()
    while (iter.hasNext) {
      iter.next()
      val bits = iter.take(5).flatMap(line =>
        line.split("""\s+""")).filter(_.trim.nonEmpty).map(_.toInt).toIndexedSeq
      val b = Board()
      for (y <- 0 until 5; x <- 0 until 5) b.put((x, y), bits(y * 5 + x))
      set += b
    }
    (plays, set)
  }

  def solve1(input: Seq[String]): Int = {
    val (plays, boards) = loadBoard(input)
    var done = false
    var score = 0
    val playIter = plays.iterator
    while (!done) {
      val next = playIter.next()
      for (b <- boards if b.play(next)) {
        done = true
        score = b.sumUnmarked * next
      }
    }
    score
  }

  def solve2(input: Seq[String]): Int = {
    var (plays, boards) = loadBoard(input)
    var done = false
    var score = 0
    val playIter = plays.iterator
    while (!done) {
      var next = playIter.next()
      if (boards.size == 1) {
        val b = boards.head
        b.play(next)
        done = true
//        while (!done) {
//          if (b.play(next)) done = true else next = playIter.next()
//        }
        score = b.sumUnmarked * next
      } else for (b <- boards if b.play(next)) boards = boards - b
    }
    score
  }
}