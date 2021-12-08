package y2021

trait Day01 {
  def countIncreases(i: Seq[Int]): Int = i.zip(i.tail).count(ab => ab._2 > ab._1)
}