package y2023

// see https://adventofcode.com/2023/day/3
trait Day03 {
  private val numberPattern = "\\d+".r

  def solvePart1(input: Seq[String]): Int = {
    val width = input.head.length
    val height = input.length

    // y, x, length
    val numberPositions: Seq[(Int, Int, Int)] = input.indices.flatMap { y =>
      numberPattern.findAllMatchIn(input(y)).map(m => (m.start, m.end - m.start)).toSeq
        .map(result => (y, result._1, result._2))
    }
    numberPositions.map { case (y, x, length) =>
      val iter = neighborIterator(x, y, length, width, height)
      if (iter.exists { case Position(x, y) => input(y)(x) != '.' && !input(y)(x).isDigit }) {
        input(y).slice(x, x + length).toInt
      } else 0
    }.sum
  }

  def solvePart2(input: Seq[String]): Int = {
    val width = input.head.length
    val height = input.length

    // y, x, length
    val numberPositions: Seq[(Int, Int, Int)] = input.indices.flatMap { y =>
      numberPattern.findAllMatchIn(input(y)).map(m => (m.start, m.end - m.start)).toSeq
        .map(result => (y, result._1, result._2))
    }
    val joins: Seq[(Position, Int)] = numberPositions.flatMap { case (y, x, length) =>
      neighborIterator(x, y, length, width, height)
        .filter { case Position(x, y) => input(y)(x) == '*' }
        .map { pos => pos -> input(y).slice(x, x + length).toInt }
        .toSeq
    }
    joins.groupBy(_._1).filter(_._2.size > 1).view.mapValues(_.map(_._2).product).values.sum
  }

  def neighborIterator(x: Int, y: Int, width: Int, gridWidth: Int, gridHeight: Int): Iterator[Position] = {
    val piece1 = for (i <- x - 1 to x + width) yield Position(x = i, y = y - 1)
    val piece2 = for (i <- x - 1 to x + width) yield Position(x = i, y = y + 1)
    val piece3 = Seq(Position(x = x - 1, y = y), Position(x = x + width, y = y))
    Seq(piece1, piece2, piece3).flatten.iterator.filter { case Position(x, y) =>
      x >= 0 && x < gridWidth && y >= 0 && y < gridHeight
    }
  }

  case class Position(x: Int, y: Int)
}
