package y2023

trait Day06 {
  private def calculateWinningWays(time: Long, distance: Long): Long = {
    // Find number of ways to beat the distance
    // Using quadratic: t * (time - t) > distance
    // -t^2 + time*t - distance > 0
    val discriminant = math.sqrt(time * time - 4.0 * distance)
    val x1 = (time - discriminant) / 2.0
    val x2 = (time + discriminant) / 2.0

    val start = math.ceil(x1 + 0.000001).toLong
    val end = math.floor(x2 - 0.000001).toLong
    end - start + 1
  }

  def solvePart1(input: Seq[String]): Long = {
    val times = input.head.split(":")(1).trim.split("\\s+").map(_.toLong)
    val distances = input(1).split(":")(1).trim.split("\\s+").map(_.toLong)
    times.zip(distances).map(calculateWinningWays.tupled).product
  }

  def solvePart2(input: Seq[String]): Long = {
    val time = input.head.split(":")(1).filterNot(_.isWhitespace).toLong
    val distance = input(1).split(":")(1).filterNot(_.isWhitespace).toLong
    calculateWinningWays(time, distance)
  }
}