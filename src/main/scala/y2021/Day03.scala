package y2021

trait Day03 {

  def toBinary(i: String, x: Int = 0): Int = {
    if (i.isEmpty) x
    else if (i(0) == '1') toBinary(i.tail, (x << 1) + 1)
    else toBinary(i.tail, x << 1)
  }

  def solve1(input: Seq[String]): Int = {
    val bits = input.head.length
    val accum = Array.fill[Int](bits)(0)
    for (i <- input; j <- 0 until i.length)
      i(j) match {
        case '0' => accum(j) = accum(j) - 1
        case '1' => accum(j) = accum(j) + 1
      }

    def recurse(i: Array[Int], b: StringBuilder = new StringBuilder): String = {
      if (i.isEmpty) b.toString()
      else {
        if (i(0) > 0) recurse(i.tail, b.append(("1")))
        else recurse(i.tail, b.append("0"))
      }
    }

    def flip(s: String): String = {
      def r(x: String, b: StringBuilder = new StringBuilder): String = {
        if (x.isEmpty) b.toString()
        else if (x(0) == '1') r(x.substring(1), b.append("0"))
        else r(x.substring(1), b.append("1"))
      }

      r(s)
    }

    val a = recurse(accum)
    val x = toBinary(a)
    val b = flip(a)
    val y = toBinary(b)
    x * y
  }

  def solve2(s: Seq[String], c: Char, i: Int = 0): String = {
    def counts(s: Seq[String], i: Int, c: Char): Int = s.map(_ (i)).count(_ == c)

    if (s.length == 1) s.head
    else {
      val ones = counts(s, i, c)
      val zeros = s.length - ones
      if ((c == '1' && ones >= zeros) || (c == '0' && ones <= zeros))
        solve2(s.filter(_ (i) == c), c, i + 1)
      else
        solve2(s.filter(_ (i) != c), c, i + 1)
    }
  }
}