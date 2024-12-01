package y2024

trait Day01:
  private def parseLine(line: String): (Int, Int) =
    line.trim.split("\\s+") match
      case Array(left, right) => (left.toInt, right.toInt)
      case _ => throw IllegalArgumentException(s"Invalid input line: $line")

  def solvePart1(input: Seq[String]): Int =
    val pairs = input.map(parseLine)
    val (left, right) = (pairs.map(_._1).sorted, pairs.map(_._2).sorted)
    left.zip(right).map((l, r) => math.abs(l - r)).sum

  def solvePart2(input: Seq[String]): Int =
    val pairs = input.map(parseLine)
    val (left, right) = (pairs.map(_._1), pairs.map(_._2))
    val rightCounts = right.groupBy(identity).view.mapValues(_.size).toMap
    left.map(num => num * rightCounts.getOrElse(num, 0)).sum
end Day01