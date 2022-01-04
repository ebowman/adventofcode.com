package y2016

import scala.annotation.tailrec

trait Day08 {

  object Display {
    val Rect = """rect (\d+)x(\d+)""".r
    val Column = """rotate column x=(\d+) by (\d+)""".r
    val Row = """rotate row y=(\d+) by (\d+)""".r

    def apply(width: Int = 50, height: Int = 6) =
      new Display(Array.ofDim[Boolean](height, width).map(_.toIndexedSeq).toIndexedSeq)
  }

  case class Display(pixels: IndexedSeq[IndexedSeq[Boolean]]) {
    def rect(x: Int, y: Int): Display = {
      @tailrec def recurse(display: Display, x1: Int = 0, y1: Int = 0): Display = {
        if (y1 == y) display
        else if (x1 == x) recurse(display, 0, y1 + 1)
        else recurse(display.copy(pixels = display.pixels.updated(y1, display.pixels(y1).updated(x1, true))), x1 + 1, y1)
      }

      recurse(this, 0, 0)
    }

    def rotateColumn(x: Int, n: Int): Display = {
      @tailrec def recurse(display: Display, y: Int = 0): Display =
        if (y == pixels.size) display
        else recurse(display.copy(
          pixels = display.pixels.updated(y,
            display.pixels(y).updated(x, pixels((y + pixels.size - n) % pixels.size)(x)))), y + 1)

      recurse(this)
    }

    def rotateRow(y: Int, n: Int): Display = {
      @tailrec def recurse(display: Display, x: Int = 0): Display =
        if (x == pixels.head.size) display
        else recurse(display.copy(
          pixels = display.pixels.updated(y,
            display.pixels(y).updated(x, pixels(y)((x + pixels.head.size - n) % pixels.head.size)))), x + 1)

      recurse(this)
    }

    def execute(instruction: String): Display = {
      import Display._
      instruction match {
        case Rect(x, y) => rect(x.toInt, y.toInt)
        case Row(y, n) => rotateRow(y.toInt, n.toInt)
        case Column(x, n) => rotateColumn(x.toInt, n.toInt)
      }
    }

    def lit: Int = pixels.map(row => row.count(_ == true)).sum

    override def toString: String = pixels.map(row => row.map(p => if (p) 'X' else ' ').mkString("|")).mkString("\n")
  }

  def solve(input: Seq[String]): Display = input.foldLeft(Display()) {
    case (display, input) => display.execute(input)
  }
}