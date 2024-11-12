package y2021

import scala.collection.mutable

trait Day20 {
  def solve(input: Seq[String], n: Int): Int = {
    val algo = input.head
    val image = input.tail.tail
    val inputImage = mutable.Set[(Int, Int)]()
    for y <- image.indices; x <- 0 until image.head.length if image(y)(x) == '#' do inputImage.add((x, y))
    var empty = '.'
    (0 until n).foldLeft(inputImage) { case (inputImage, _) =>
      val (minX, minY) = (inputImage.minBy(_._1)._1 - 1, inputImage.minBy(_._2)._2 - 1)
      val (maxX, maxY) = (inputImage.maxBy(_._1)._1 + 1, inputImage.maxBy(_._2)._2 + 1)
      val outputImage = inputImage.clone()
      for y <- minY to maxY; x <- minX to maxX do {
        var index = 0
        for y1 <- -1 to 1; x1 <- -1 to 1 do {
          val inSpace = minX >= x + x1 || maxX <= x + x1 || minY >= y + y1 || maxY <= y + y1
          index = (index << 1) | (if empty == '#' && inSpace || inputImage((x + x1, y + y1)) then 1 else 0)
        }
        (if algo(index) == '#' then (z: (Int, Int)) => outputImage.add(z) else (z: (Int, Int)) => outputImage.remove(z)) ((x, y))
      }
      empty = if empty == '#' then algo.last else algo.head
      outputImage
    }.size
  }
}
