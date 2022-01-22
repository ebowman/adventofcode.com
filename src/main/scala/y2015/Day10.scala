package y2015

trait Day10 {

  @scala.annotation.tailrec
  final def iterate(str: String, count: Int): String = {
    if count == 0 then str
    else iterate(speaknsay(str), count - 1)
  }

  def speaknsay(s: String): String = {
    var cur: Char = 0
    var count: Int = 0

    @scala.annotation.tailrec
    def recurse(s: String, cursor: Int, b: StringBuilder): StringBuilder = {
      if cursor == s.length then {
        if count > 0 then {
          b.append(count)
          b.append(cur)
        }
        b
      }
      else {
        if s(cursor) == cur then {
          count += 1
        } else {
          if count > 0 then {
            b.append(count)
            b.append(cur)
          }
          cur = s(cursor)
          count = 1
        }
        recurse(s, cursor + 1, b)
      }
    }

    recurse(s, 0, new StringBuilder).toString
  }
}
