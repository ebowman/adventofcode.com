package y2016

trait Day20 {
  def solve1(input: Seq[String]): (Long, Long) = {
    case class Range(min: Long, max: Long)
    val range = input.map(line => Range(line.split("-").head.toLong, line.split("-")(1).toLong)).sortBy(_.min)
    val candidates = range.map(_.max + 1).filter(n => !range.exists(r => n >= r.min && n <= r.max))
    (candidates.min, candidates.flatMap { candidate =>
      range.zip(range.tail).collect {
        case (r1, r2) if candidate > r1.max && candidate < r2.min => r2.min - candidate
      }
    }.sum)
  }
}
