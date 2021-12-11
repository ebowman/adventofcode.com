package y2021

trait Day05 {
  case class Line(a: (Int, Int), b: (Int, Int)) {
    def vert: Boolean = a._1 == b._1

    def horiz: Boolean = a._2 == b._2

    def diag: Boolean = math.abs(b._1 - a._1) == math.abs(b._2 - a._2)

    def vertOrHoriz: Boolean = vert || horiz

    def vertOrHorizOrDiag: Boolean = vertOrHoriz || diag

    def visit(f: ((Int, Int)) => Unit): Unit = {
      val delta =
        if (a._1 == b._1)
          if (a._2 < b._2) (0, 1)
          else (0, -1)
        else if (a._2 == b._2)
          if (a._1 < b._1) (1, 0)
          else (-1, 0)
        else if (a._1 < b._1)
          if (a._2 < b._2) (1, 1)
          else (1, -1)
        else if (a._2 < b._2) (-1, 1)
        else (-1, -1)
      f(a)
      var cur = (a._1 + delta._1, a._2 + delta._2)
      while (cur != b) {
        f(cur)
        cur = (cur._1 + delta._1, cur._2 + delta._2)
      }
      f(b)
    }
  }

  object Line {
    val Parse = """(\d+),(\d+) -> (\d+),(\d+)""".r

    def apply(str: String): Line = {
      str match {
        case Parse(a, b, c, d) => Line((a.toInt, b.toInt), (c.toInt, d.toInt))
      }
    }
  }

  def solve(input: Seq[String], f: Line => Boolean): Int = {
    val lines = input.map(Line.apply).filter(f)
    import math.max
    def max3(x: Int, y: Int, z: Int) = max(x, max(y, z))

    val (maxX, maxY) = lines.foldLeft((Int.MinValue, Int.MinValue)) {
      case ((maxX, maxY), line) => (max3(maxX, line.a._1, line.b._1), max3(maxY, line.a._2, line.b._2))
    }
    val blackboard = Array.ofDim[Int](maxY + 1, maxX + 1)
    lines.foreach { line =>
      line.visit {
        case (x, y) => blackboard(y)(x) += 1
      }
    }
    blackboard.map(_.count(_ > 1)).sum
  }
}