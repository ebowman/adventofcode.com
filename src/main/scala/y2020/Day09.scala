package y2020

trait Day09 {

  def part1(preamble: IndexedSeq[Long], count: Int, start: IndexedSeq[Long]): Long = {
    val combs = preamble.take(count).combinations(2)
    combs.map(ab => ab(0) + ab(1)).find(_ == start(0)) match {
      case Some(_) => part1(preamble.tail, count, start.tail)
      case None => start.head
    }
  }

  def part2(data: IndexedSeq[Long], goal: Long): Long = {

    var (a, b) = (0, 0)
    var done = false
    var r = 0L
    while (a < data.size - 1 && !done) {
      b = a + 1
      while (b < data.size && !done) {
        if (sum(data, a, b) == goal) {
          r = minMaxSum(data, a, b)
          done = true
        }
        b += 1
      }
      a += 1
    }
    r
    //    (0 until data.size - 1).flatMap { a =>
    //      (a + 1 until data.size).map(b => (a, b))
    //    }.find(ab => sum(data, ab._1, ab._2) == goal).map(ab => minMaxSum(data, ab._1, ab._2)).get
  }

  def sum(x: IndexedSeq[Long], a: Int, b: Int): Long = {
    var i = a
    var sum: Long = 0
    while (i <= b) {
      sum += x(i)
      i += 1
    }
    sum
  }

  def minMaxSum(x: IndexedSeq[Long], a: Int, b: Int): Long = {
    var min = Long.MaxValue
    var max = Long.MinValue
    var i = a
    while (i <= b) {
      min = Math.min(min, x(i))
      max = Math.max(max, x(i))
      i += 1
    }
    min + max
  }

}
