package y2021

import scala.annotation.tailrec

trait Day12 {

  @tailrec final def parseInput(input: Seq[String],
                                edges: Map[String, Seq[String]] = Map()): Map[String, Seq[String]] = {
    if (input.isEmpty) edges
    else {
      val Array(x, y) = input.head.split("\\-")
      parseInput(input.tail,
        (edges + (x -> (edges.getOrElse(x, Seq()) :+ y))) + (y -> (edges.getOrElse(y, Seq()) :+ x)))
    }
  }

  def solve1(input: Seq[String]): Int = {
    val edges = parseInput(input)
    var queue: Seq[Seq[String]] = Seq(Seq("start"))
    var count = 0
    while (queue.nonEmpty) {
      val path: Seq[String] = queue.head
      queue = queue.tail
      for (next: String <- edges(path.last)) {
        if (next == "end") count += 1
        else if (!next.forall(_.isLower) || !path.contains(next)) {
          queue :+= (path :+ next)
        }
      }
    }
    count
  }

  def solve2(input: Seq[String]): Int = {
    val edges = parseInput(input)
    var queue = Seq(Seq("start"))
    var count = 0
    while (queue.nonEmpty) {
      val path = queue.head
      queue = queue.tail
      for (next <- edges(path.last)) {
        val backPressure = if (next.forall(_.isLower) && path.contains(next)) Seq("<") else Seq.empty
        if (next == "end") count += 1
        else if (next != "start" && path.head != "<" || backPressure.isEmpty) {
          queue :+= ((backPressure ++ path) :+ next)
        }
      }
    }
    count
  }
}