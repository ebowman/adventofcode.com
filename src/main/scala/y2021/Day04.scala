package y2021

trait Day04 {
  case class Board(x: Array[Int] = Array.fill(100)(-1),
                   y: Array[Int] = Array.fill(100)(-1),
                   marks: Array[Boolean] = Array.fill(100)(false)) {

    override def toString: String = {
      val b = new StringBuilder
      for (y <- 0 until 5) {
        for (x <- 0 until 5) {
          val s = valOf(x, y).get
          if (isMarked((x, y)))
            b.append("*").append(valOf(x, y).get).append("*").append(" ")
          else
            b.append(valOf(x, y).get).append(" ")
        }
        b.append("\n")
      }
      b.toString
    }

    def put(c: (Int, Int), v: Int): Unit = {
      x(v) = c._1
      y(v) = c._2
    }

    def coordOf(v: Int): (Int, Int) = (x(v), y(v))

    def isMarked(v: Int) = marks(v)

    def isMarked(c: (Int, Int)) = valOf(c).map(marks).getOrElse(false)

    def valOf(c: (Int, Int)): Option[Int] =
      (0 until 100).find(i => x(i) == c._1 && y(i) == c._2)

    def mark(m: Int) = marks(m) = true

    def sumUnmarked: Int = {
      (0 until 100).filter(i => x(i) != -1).filterNot(isMarked).sum
    }

    def play(v: Int): Boolean = {
      val c = coordOf(v)
      if (c == (-1, -1)) false
      else {
        mark(v)
      }
      isBingo
    }

    def isBingo: Boolean = {
      var found = false
      for (x <- 0 until 5) {
        if (!found) {
          val c = (0 until 5).map(y => (x, y)).count(isMarked)
          found = c == 5
          //found = (0 until y.length).map(y => (x, y)).count(isMarked) == y.length
        }
      }
      if (!found) {
        for (y <- 0 until 5) {
          if (!found) {
            val c = (0 until 5).map(x => (x, y)).count(isMarked)
            found = c == 5
            //found = (0 until x.length).map(x => (x, y)).count(isMarked) == x.length
          }
        }
      }
      found
    }
  }

  def loadBoard(lines: Iterable[String]): (Seq[Int], Set[Board]) = {
    val iter = lines.iterator
    val plays: Seq[Int] = iter.next().split(",").map(_.toInt)
    var set = Set[Board]()
    while (iter.hasNext) {
      iter.next()
      val b = Board()
      val bits = iter.take(5).flatMap(line =>
        line.split("\\s+")).filter(_.trim.length > 0).map(_.toInt).toIndexedSeq
      for (y <- 0 until 5; x <- 0 until 5) {
        b.put((x, y), bits(y * 5 + x))
      }
      set = set + b
    }
    (plays, set)
  }
}