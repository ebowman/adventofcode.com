package y2015

trait Day18 {
  def parse(input: Iterable[String], pinned: Boolean = false): Grid = {
    val array = mkGrid(input.head.length)
    val inputIter = input.iterator
    val rowIter = array.iterator
    rowIter.next() // skip the top slop row
    while (inputIter.hasNext) {
      val inputRow = inputIter.next()
      val row = rowIter.next()
      for (i <- 1 to inputRow.length) {
        row(i) = inputRow(i - 1) match {
          case '#' => true
          case '.' => false
        }
      }
    }
    val dim = array.length - 2
    if (pinned) {
      array(1)(1) = true
      array(dim)(1) = true
      array(1)(dim) = true
      array(dim)(dim) = true
    }
    Grid(dim, array, pinned)
  }

  def mkGrid(dim: Int): Array[Array[Boolean]] = Array.ofDim[Boolean](dim + 2, dim + 2)

  case class Grid(dim: Int, board: Array[Array[Boolean]], pinned: Boolean = false) {

    assert(dim == board.length - 2)

    def next: Grid = {
      val nextBoard = mkGrid(dim)
      for {y <- 1 to dim
           x <- 1 to dim} {
        val lit = Cursor(x, y).lit
        nextBoard(y)(x) = if (board(y)(x)) lit == 2 || lit == 3 else lit == 3
      }
      if (pinned) {
        nextBoard(1)(1) = true
        nextBoard(1)(dim) = true
        nextBoard(dim)(1) = true
        nextBoard(dim)(dim) = true
      }
      Grid(dim, nextBoard, pinned)
    }

    def lit: Int = board.map(_.count(_ == true)).sum

    case class Cursor(x: Int, y: Int) extends Iterable[Boolean] {

      def lit: Int = this.count(x => x)

      override def iterator: Iterator[Boolean] = new Iterator[Boolean] {
        private var (cx, cy) = (-1, -1)

        override def hasNext: Boolean = !(cx == 2 && cy == 2)

        override def next(): Boolean = {
          val result = board(y + cy)(x + cx)
          (cy, cx) match {
            case (-1, -1) => cy = -1; cx = 0
            case (-1, 0) => cy = -1; cx = 1
            case (-1, 1) => cy = 0; cx = -1
            case (0, -1) => cy = 0; cx = 1
            case (0, 1) => cy = 1; cx = -1
            case (1, -1) => cy = 1; cx = 0
            case (1, 0) => cy = 1; cx = 1
            case (1, 1) => cy = 2; cx = 2
          }
          result
        }
      }
    }
  }

}