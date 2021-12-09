package y2021

trait Day07 {
  def solve(input: String): Int = {
    val crabs = input.split(",").map(_.toInt)
    var min = Int.MaxValue
    for (i <- crabs.min to crabs.max) {
      var pos = 0
      for (j <- crabs if i != j) pos += math.abs(i - j)
      min = math.min(min, pos)
    }
    min
  }

  def solve2(input: String): Int = {
    val crabs = input.split(",").map(_.toInt).toSeq
    var min = Int.MaxValue
    for (i <- crabs.min to crabs.max) {
      var pos = 0
      for (j <- crabs if i != j) {
        val d = math.abs(i - j)
        pos += (d * (d + 1)) / 2
      }
      min = math.min(min, pos)
    }
    min
  }
}