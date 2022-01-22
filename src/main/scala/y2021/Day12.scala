package y2021

import scala.annotation.tailrec
import scala.collection.mutable

trait Day12 {

  @tailrec final def parseInput(input: Seq[String],
                                edges: Map[String, Seq[String]] = Map()): Map[String, Seq[String]] = {
    if input.isEmpty then edges
    else {
      val Array(x, y) = input.head.split("-")
      parseInput(input.tail,
        (edges + (x -> (edges.getOrElse(x, Seq()) :+ y))) + (y -> (edges.getOrElse(y, Seq()) :+ x)))
    }
  }

  def solve1(input: Seq[String]): Int = {
    val edges = parseInput(input)
    val queue = mutable.Queue(Seq("start"))
    var count = 0
    while queue.nonEmpty do {
      val path = queue.dequeue()
      for next <- edges(path.last) do {
        if next == "end" then count += 1
        else if !next.forall(_.isLower) || !path.contains(next) then {
          queue.enqueue(path :+ next)
        }
      }
    }
    count
  }

  def solve2(input: Seq[String]): Int = {
    val edges = parseInput(input)
    val queue = mutable.Queue(Seq("start"))
    var count = 0
    while queue.nonEmpty do {
      val path = queue.dequeue()
      for next <- edges(path.last) do {
        val backPressure = if next.forall(_.isLower) && path.contains(next) then Seq("<") else Seq.empty
        if next == "end" then count += 1
        else if next != "start" && path.head != "<" || backPressure.isEmpty then
          queue.enqueue((backPressure ++ path) :+ next)
      }
    }
    count
  }
}