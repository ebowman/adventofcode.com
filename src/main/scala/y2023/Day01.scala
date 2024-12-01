
package y2023

// see https://adventofcode.com/2023/day/1
class Day01 extends util.Day(1):

  private val toMap = List[(String, String)](
    "one" -> "o1e",
    "two" -> "t2o",
    "three" -> "th3ee",
    "four" -> "f4ur",
    "five" -> "f5ve",
    "six" -> "s6x",
    "seven" -> "s7ven",
    "eight" -> "e8ght",
    "nine" -> "n9ne",
  )

  def fix(str: IndexedSeq[String]): IndexedSeq[String] =
    str.map: row =>
      toMap.foldLeft(row)((acc, pair) => acc.replace(pair._1, pair._2))

  def solvePart1(input: IndexedSeq[String]): Any =
    input.map: row =>
      val rev = row.reverse
      s"${row(row.indexWhere(_.isDigit))}${rev(rev.indexWhere(_.isDigit))}".toInt
    .sum

  def solvePart2(input: IndexedSeq[String]): Any = solvePart1(fix(input))
