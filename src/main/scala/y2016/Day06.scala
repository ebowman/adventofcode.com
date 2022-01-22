package y2016

import scala.annotation.tailrec

trait Day06 {
  def solve(input: Seq[String], f: Map[Char, Int] => Char): String = {
    @tailrec def recurse(lines: Seq[String], i: Int, counters: IndexedSeq[Map[Char, Int]]): String =
      if i == 8 then recurse(lines.tail, 0, counters)
      else if lines.isEmpty then counters.map(f).mkString
      else recurse(lines, i + 1, counters.updated(i, counters(i) + (lines.head(i) -> (counters(i)(lines.head(i)) + 1))))

    recurse(input, 0, IndexedSeq.fill(8)(Map[Char, Int]().withDefaultValue(0)))
  }
}