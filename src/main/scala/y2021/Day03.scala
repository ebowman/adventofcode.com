package y2021

import scala.annotation.tailrec

trait Day03 {
  @tailrec final def toBinary(i: String, x: Int = 0): Int =
    if (i.isEmpty) x
    else toBinary(i.tail, (x << 1) + (i(0) - '0'))

  def solve1(input: Seq[String]): Int = {
    @tailrec def recurse(i: Array[Int], b: StringBuilder = new StringBuilder): String = {
      if (i.isEmpty) b.toString()
      else recurse(i.tail, b.append(if (i(0) > 0) '1' else '0'))
    }

    @tailrec def flip(s: String, b: StringBuilder = new StringBuilder): String = {
      if (s.isEmpty) b.toString()
      else flip(s.tail, b.append(('a' - s(0)).toChar))
    }

    val accum = Array.fill[Int](input.head.length)(0)
    for (line <- input; i <- line.indices) accum(i) += 2 * (line(i) - '0') - 1
    val a = recurse(accum)
    toBinary(a) * toBinary(flip(a))
  }

  @tailrec final def solve2(s: Seq[String], c: Char, i: Int = 0): Int =
    if (s.length == 1) toBinary(s.head)
    else {
      val ones = 2 * s.map(_ (i)).count(_ == c)
      solve2(s.filter {
        if (c == '1' && ones >= s.length || c == '0' && ones <= s.length)
          _ (i) == c
        else
          _ (i) != c
      }, c, i + 1)
    }
}