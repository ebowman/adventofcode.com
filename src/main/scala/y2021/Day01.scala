package y2021

trait Day01 {
  def countIncreases(i: Seq[Int]): Int = i.zip(i.tail).count(ab => ab._2 > ab._1)

  def windowIncreases(i: Seq[Int]): Int = countIncreases(i.sliding(3).take(3 * (i.size / 3)).map(_.sum).toSeq)
}