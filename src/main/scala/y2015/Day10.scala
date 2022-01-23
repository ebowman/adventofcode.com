package y2015

import scala.annotation.tailrec

trait Day10 {
  @tailrec final def solve(str: String, count: Int): String =
    if count == 0 then str
    else solve(speaknsay(str), count - 1)

  def speaknsay(s: String): String =
    var cur: Char = 0
    var count: Int = 0
    val sb = new StringBuilder

    @tailrec def recurse(s: String, cursor: Int): String =
      if cursor == s.length then
        if count > 0 then sb.append(count).append(cur)
        sb.toString
      else
        if s(cursor) == cur then count += 1
        else
          if count > 0 then sb.append(count).append(cur)
          cur = s(cursor)
          count = 1
        recurse(s, cursor + 1)

    recurse(s, 0)
}
