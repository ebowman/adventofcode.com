
package y2023

import scala.annotation.tailrec

// see https://adventofcode.com/2023/day/1
trait Day01 {

  val toMap = List[(String, String)](
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

  val revMap = toMap.map(kv => kv._1.reverse -> kv._2)

  def fix(str: String): String =
    toMap.foldLeft(str)((acc, pair) => acc.replace(pair._1, pair._2))

  def solve(input: String): Int = {
    val rev = input.reverse
    s"${input(input.indexWhere(_.isDigit))}${rev(rev.indexWhere(_.isDigit))}".toInt
  }

  def solve2(input: String): Int = solve(fix(input))
}
