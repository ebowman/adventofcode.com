package y2019

trait Day16 {
  object WaveGenerator {
    val pattern = IndexedSeq(0, 1, 0, -1)
  }
  case class WaveGenerator(n: Int) {
    var cursor = 1
    def next(): Int = {
      val rtn = WaveGenerator.pattern((cursor / n) % 4)
      cursor += 1
      rtn
    }
    def skip(i: Int): Unit = cursor += i
  }

  def part1(input: String, n: Int): String = {
    var accum = input
    for (_ <- 1 to n) {
      accum = (for (g <- 1 to accum.length) yield {
        val wg = WaveGenerator(g)
        wg.skip(g - 1)
        (for (i <- g - 1 until accum.length) yield s"${accum(i)}".toInt * wg.next()).sum.toString.last
      }).mkString
    }
    accum.take(8)
  }

  def part2(input: String, n: Int): String = {
    var accum = (input * 10000).toCharArray
    for (_ <- 1 to n) {
      val b = new StringBuilder
      var acc = 0
      for (i <- accum.indices) {
        val n = accum(accum.length - i - 1) - '0'
        acc += n
        b.append(((acc % 10) + '0').toChar)
      }
      accum = b.reverse.toCharArray
    }
    val offset = input.take(7).toInt
    new String(accum.slice(offset, offset + 8))
  }
}