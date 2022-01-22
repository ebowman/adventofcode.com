package y2016

trait Day03 {
  val R = """\s*(\d+)\s+(\d+)\s+(\d+)""".r

  def solve1(input: Seq[String]): Int = {
    input.map {
      case R(s1, s2, s3) =>
        val sides = Seq(s1.toInt, s2.toInt, s3.toInt).sorted
        if sides.init.sum > sides.last then 1 else 0
    }.sum
  }

  def solve2(input: Seq[String]): Int = {
    val (t1, t2, t3) = input.foldLeft((List.empty[Int], List.empty[Int], List.empty[Int])) {
      case ((t1, t2, t3), R(s1, s2, s3)) => (s1.toInt :: t1, s2.toInt :: t2, s3.toInt :: t3)
      case (t, _) => t
    }
    (t1 ::: t2 ::: t3).grouped(3).count { t => t.sum > 2 * t.max }
  }
}