package y2017

trait Day02:
  private val numRx = """\d+""".r

  def solve1(str: Seq[String]): Int = str.map {
    row => numRx.findAllMatchIn(row).map(_.group(0).toInt).toSeq
  }.foldLeft(0) { (sum, row) => sum + row.max - row.min }

  def solve2(str: Seq[String]): Int = str.map(row => numRx.findAllMatchIn(row).map(_.group(0).toInt).toSeq)
    .flatMap(row => row.flatMap(i => row.withFilter(j => i != j && (i % j) == 0).map(j => i / j))).sum
  
end Day02
