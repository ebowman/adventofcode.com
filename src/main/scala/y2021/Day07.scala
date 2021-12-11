package y2021

trait Day07 {
  def solve1(input: String): Int = {
    val crabs = input.split(",").map(_.toInt)
    (crabs.min to crabs.max).foldLeft(Int.MaxValue) {
      case (min, i) => math.min(min, (for (j <- crabs if i != j) yield math.abs(i - j)).sum)
    }
  }

  def solve2(input: String): Int = {
    val crabs = input.split(",").map(_.toInt)
    (crabs.min to crabs.max).foldLeft(Int.MaxValue) {
      case (min, i) => math.min(min, crabs.withFilter(j => i != j).map { j =>
        val d = math.abs(i - j)
        (d * (d + 1)) / 2
      }.sum)
    }
  }
}