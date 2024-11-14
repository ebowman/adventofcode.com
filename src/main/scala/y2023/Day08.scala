package y2023

import scala.annotation.tailrec

trait Day08 {
  def solvePart1(input: Seq[String]): Int = {
    val (directions, nodes) = parseInput(input)
    findSteps(directions, nodes, "AAA", _.equals("ZZZ")).toInt
  }

  def solvePart2(input: Seq[String]): Long = {
    val (directions, nodes) = parseInput(input)
    val startNodes = nodes.keys.filter(_.endsWith("A"))
    startNodes.map(findSteps(directions, nodes, _, _.endsWith("Z"))).reduce(lcm)
  }

  private def parseInput(input: Seq[String]) = {
    val directions = input.head.trim
    val nodePattern = """(\w+) = \((\w+), (\w+)\)""".r
    val nodes = input.drop(2).map { case nodePattern(node, left, right) => node -> (left, right) }.toMap
    (directions, nodes)
  }

  @tailrec
  private def findSteps(directions: String, nodes: Map[String, (String, String)],
                        start: String, isEnd: String => Boolean,
                        steps: Long = 0, dirIndex: Int = 0): Long = {
    if (isEnd(start)) steps
    else {
      val next = if (directions(dirIndex % directions.length) == 'L') nodes(start)._1 else nodes(start)._2
      findSteps(directions, nodes, next, isEnd, steps + 1, dirIndex + 1)
    }
  }

  private def lcm(a: Long, b: Long): Long = (a * b) / gcd(a, b)

  @tailrec
  private def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)
}