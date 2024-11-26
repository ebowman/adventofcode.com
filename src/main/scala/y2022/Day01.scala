package y2022

trait Day01:
  def solvePart1(input: Seq[String]): Int =
    parseElves(input).max

  def solvePart2(input: Seq[String]): Int =
    parseElves(input).sorted(Ordering[Int].reverse).take(3).sum

  private def parseElves(input: Seq[String]): Seq[Int] =
    input
      .foldLeft(Seq(Seq.empty[String]))((acc, line) =>
        if line.isEmpty then
          Seq.empty[String] +: acc
        else
          (line +: acc.head) +: acc.tail
      )
      .map(_.map(_.toInt).sum)

end Day01
