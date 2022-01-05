package y2016

import scala.annotation.tailrec
import scala.util.matching.Regex

trait Day09 {

  private val Marker = """\((\d+)x(\d+)\)""".r
  private val Marker2 = """\((\d+)x(\d+)\).*""".r

  def solve1(input: String): Int = {
    @tailrec def recurse(i: String, sb: StringBuilder = new StringBuilder): String = {
      if (i.isEmpty) sb.toString
      else {
        Marker.findFirstMatchIn(i) match {
          case Some(m) =>
            sb.append(i.take(m.start))
            i.drop(m.start) match {
              case Marker2(x, y) =>
                val toAppend = i.slice(m.end, m.end + x.toInt) * y.toInt
                sb.append(toAppend)
                recurse(i.drop(m.end + x.toInt), sb)
            }
          case None =>
            sb.append(i)
            sb.toString()
        }
      }
    }

    recurse(input).length
  }

  def solve2(input: String): Long = {
    if (!input.contains("(")) input.length
    else {
      var len = 0L
      var working = input
      while (working.contains("(")) {
        val open = working.indexOf("(")
        val close = working.indexOf(")")
        val marker = working.slice(open + 1, close).split("x").map(_.toInt)
        len += open + solve2(working.slice(close + 1, close + 1 + marker(0)) * marker(1))
        working = working.drop(close + 1 + marker(0))
      }
      len + working.length
    }
  }
}