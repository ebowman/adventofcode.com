package y2017

trait Day04 {
  def valid1(line: String): Boolean =
    val bits = line.split(" ")
    bits.length == bits.distinct.length

  def solve1(input: Seq[String]): Int = input.count(valid1)

  def valid2(line: String): Boolean =
    val bits = line.split(" ").map(_.sorted)
    bits.length == bits.distinct.length

  def solve2(input: Seq[String]): Int = input.count(valid2)
}
