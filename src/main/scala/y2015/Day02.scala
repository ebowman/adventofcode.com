package y2015

trait Day02 {
  object Dims {
    val Rx = """(\d+)x(\d+)x(\d+)""".r

    def apply(str: String): Dims = str match {
      case Rx(l, w, h) => Dims(l.toInt, w.toInt, h.toInt)
    }
  }

  case class Dims(l: Int, w: Int, h: Int) {
    val total: Int = 2 * (l * w + w * h + l * h) + math.min(math.min(l * w, w * h), l * h)
    val ribbon: Int = Seq(2 * l, 2 * w, 2 * h).sorted.take(2).sum + l * w * h
  }

  def solve1(input: Iterable[String]): Int = input.map(Dims.apply).map(_.total).sum

  def solve2(input: Iterable[String]): Int = input.map(Dims.apply).map(_.ribbon).sum
}
