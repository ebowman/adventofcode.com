package y2022

trait Day04:
  case class Range(start: Int, end: Int):
    def contains(other: Range): Boolean =
      start <= other.start && end >= other.end

    def overlaps(other: Range): Boolean =
      !(end < other.start || start > other.end)

  object Range:
    def fromString(s: String): Range =
      s.split("-") match
        case Array(start, end) => Range(start.toInt, end.toInt)
        case _ => throw IllegalArgumentException(s"Invalid range format: $s")

  case class Pair(first: Range, second: Range):
    def hasFullContainment: Boolean =
      first.contains(second) || second.contains(first)

    def hasOverlap: Boolean =
      first.overlaps(second)

  object Pair:
    def fromString(s: String): Pair =
      s.split(",") match
        case Array(r1, r2) => Pair(Range.fromString(r1), Range.fromString(r2))
        case _ => throw IllegalArgumentException(s"Invalid pair format: $s")

  def parsePairs(input: Seq[String]): Seq[Pair] =
    input.map(Pair.fromString)

  def solvePart1(input: Seq[String]): Int =
    parsePairs(input).count(_.hasFullContainment)

  def solvePart2(input: Seq[String]): Int =
    parsePairs(input).count(_.hasOverlap)
end Day04
