package y2016

import scala.annotation.tailrec

trait Day19 {

  def solve1(n: Int): Int = {
    val elves = Array.fill(n)(1)

    @tailrec def recurse(): Int = {
      val winner = elves.indexOf(n)
      if (winner >= 0) winner
      else {
        var i = 0
        while (i < n) {
          if (elves(i) != 0) {
            val next = elves.indexWhere(_ != 0, i + 1) match {
              case -1 => elves.indexWhere(_ != 0, 0)
              case n => n
            }
            elves(i) += elves(next % n)
            elves(next % n) = 0
          }
          i += 1
        }
        recurse()
      }
    }

    recurse() + 1
  }

  def solve2(n: Int): Int = {
    val fast = true
    if (fast) {
      @tailrec def recurse(i: Int): Int =
        if (i * 3 < n) recurse(i * 3)
        else n - i
      recurse(1)
    } else {
      val values = Array.fill(n)(1)
      val next = { val nx = Array.range(1, n + 1); nx(nx.length - 1) = 0; nx }
      val prev = { val pr = Array.range(-1, n - 1); pr(0) = pr.length - 1; pr }
      var count = n

      @inline @tailrec def advance(c: Int, n: Int): Int = if (n == 0) c else advance(next(c), n - 1)
      @inline def opposite(c: Int): Int = advance(c, count / 2)
      var cursor = 0
      while (values.indexOf(n) == -1) {
        if (count > 0 && (count % 1000) == 0) println(count)
        val opp = opposite(cursor)
        values(cursor) += values(opp)
        values(opp) = 0
        next(prev(opp)) = next(opp)
        prev(next(opp)) = prev(opp)
        next(opp) = -1
        prev(opp) = -1
        cursor = next(cursor)
        count -= 1
      }
      values.indexOf(n) + 1
    }
  }
}