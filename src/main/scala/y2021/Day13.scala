package y2021

import annotation.tailrec

trait Day13 {

  def load(input: Seq[String]): (Set[(Int, Int)], Seq[(Char, Int)]) = {
    val Pair = """(\d+),(\d+)""".r
    val Fold = """fold along (.*?)=(\d+)""".r

    @tailrec def recurse(input: Seq[String],
                         points: Set[(Int, Int)] = Set(),
                         folds: Seq[(Char, Int)] = Seq()): (Set[(Int, Int)], Seq[(Char, Int)]) = {
      if input.isEmpty then (points, folds)
      else {
        input.head match {
          case Pair(x, y) => recurse(input.tail, points + ((x.toInt, y.toInt)), folds)
          case Fold(x, y) => recurse(input.tail, points, folds :+ ((x(0), y.toInt)))
          case _ => recurse(input.tail, points, folds)
        }
      }
    }

    recurse(input)
  }

  def foldX(line: Int)(pt: (Int, Int)): (Int, Int) = (if pt._1 < line then pt._1 else 2 * line - pt._1, pt._2)

  def foldY(line: Int)(pt: (Int, Int)): (Int, Int) = (pt._1, if pt._2 < line then pt._2 else 2 * line - pt._2)

  def fold(folds: Seq[(Char, Int)], points: Set[(Int, Int)]): Set[(Int, Int)] = {
    folds.foldLeft(points) {
      case (points, ('x', line)) => points.map(foldX(line))
      case (points, ('y', line)) => points.map(foldY(line))
    }
  }

  def solve1(input: Seq[String]): Int = {
    val (points, folds) = load(input)
    fold(folds.take(1), points).size
  }

  def solve2(input: Seq[String]): String = {
    val (points, folds) = load(input)
    val result = fold(folds, points)
    val (maxX, maxY) = (result.map(_._1).max, result.map(_._2).max)
    (for y <- 0 to maxY yield {
      (for x <- 0 to maxX yield {
        if result.contains((x, y)) then "#" else " "
      }).mkString
    }).mkString("\n")
  }
}