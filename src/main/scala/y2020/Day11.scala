package y2020

import java.util

trait Day11 {

  val EMPTY = 'L'
  val OCCUPIED = '#'
  val FLOOR = '.'

  case class Seats(data: Array[Array[Char]] = Array.empty) {

    import Seats._

    def equals(that: Seats): Boolean = {
      data.nonEmpty && that.data.nonEmpty && data.zip(that.data).forall { case (x, y) => util.Arrays.equals(x, y) }
    }

    def solve1(seats: Seats = new Seats()): Int = solve(Strategy1, seats)

    def solve2(seats: Seats = new Seats()): Int = solve(Strategy2, seats)

    def solve(strategy: Strategy, seats: Seats): Int = new SolutionIterable(this, strategy).last.countOccupied

    def countOccupied: Int = (for (row <- data) yield row.count(_ == OCCUPIED)).sum

    def next(strategy: Strategy): Seats = {
      val cpy = copy()
      for {row <- 1 to data.length - 2
           col <- 1 to data.head.length - 2} {
        strategy.rule(row, col, data, cpy.data)
      }
      cpy
    }

    def copy(): Seats = Seats(data.map { row => row.clone() })

    private class SolutionIterable(seats: Seats, strategy: Strategy) extends Iterable[Seats] {
      def iterator: Iterator[Seats] = new Iterator[Seats] {
        private var prev = new Seats()
        private var working = seats

        override def hasNext: Boolean = !working.equals(prev)

        override def next(): Seats = {
          prev = working
          working = working.next(strategy)
          working
        }
      }
    }
  }

  object Seats {
    val neighbors = Seq((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))

    def apply(input: Iterable[String]): Seats = {
      val rows: Seq[String] = {
        val rawLines: Seq[String] = input.map(line => s"$FLOOR$line$FLOOR").toSeq
        val buffer = FLOOR.toString * rawLines.head.length
        (buffer +: rawLines) :+ buffer
      }

      new Seats(rows.map(_.toArray).toArray)
    }

    trait Strategy {
      def rule(row: Int, col: Int, data: Array[Array[Char]], cpy: Array[Array[Char]]): Unit
    }

    object Strategy1 extends Strategy {
      def rule(row: Int, col: Int, data: Array[Array[Char]], cpy: Array[Array[Char]]): Unit = {
        if (data(row)(col) == EMPTY && occupiedCount(row, col, data) == 0) cpy(row)(col) = OCCUPIED
        else if (data(row)(col) == OCCUPIED && occupiedCount(row, col, data) >= 4) cpy(row)(col) = EMPTY
      }

      def occupiedCount(row: Int, col: Int, data: Array[Array[Char]]): Int = {
        (for (n <- neighbors) yield {
          data(row + n._1)(col + n._2) == OCCUPIED
        }).count(_ == true)
      }
    }

    object Strategy2 extends Strategy {
      def rule(row: Int, col: Int, data: Array[Array[Char]], cpy: Array[Array[Char]]): Unit = {
        if (data(row)(col) == EMPTY && occupiedCount(data, row, col) == 0) cpy(row)(col) = OCCUPIED
        else if (data(row)(col) == OCCUPIED && occupiedCount(data, row, col) >= 5) cpy(row)(col) = EMPTY
      }

      def occupiedCount(data: Array[Array[Char]], row: Int, col: Int): Int = {
        case class Cursor(data: Array[Array[Char]], var row: Int, var col: Int, iter: (Int, Int)) extends Iterator[Boolean] {
          row += iter._1
          col += iter._2
          var done = false

          def hasNext: Boolean = {
            if (done) false
            else {
              done = row >= 0 && row < data.length && col >= 0 && col < data(0).length
              done
            }
          }

          def next(): Boolean = {
            val found = data(row)(col) == OCCUPIED
            done = found || data(row)(col) == EMPTY
            row = row + iter._1
            col = col + iter._2
            found
          }
        }

        def cursor(row: Int, col: Int, iter: (Int, Int)): Iterable[Boolean] = new Iterable[Boolean] {
          override def iterator: Iterator[Boolean] = Cursor(data, row, col, iter)
        }

        neighbors.count(iter => cursor(row, col, iter).exists(_ == true))
      }
    }

  }

}