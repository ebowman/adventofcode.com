package y2017

trait Day01:
  def solve1(str: String): Int =
    str.zip((str :+ str.head).tail).collect {
      case (a, b) if a == b => s"$a".toInt
      case _ => 0
    }.sum

  def solve2(str: String): Int =
    str.indices.map {
      case i if str(i) == str((i + str.length / 2) % str.length) => s"${str(i)}".toInt
      case _ => 0
    }.sum
end Day01
