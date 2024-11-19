package y2017

import scala.annotation.tailrec

trait Day12:
  def solvePart1(input: Seq[String]): Int =
    val graph = parseInput(input)
    findConnectedGroup(graph, 0).size

  def solvePart2(input: Seq[String]): Int =
    val graph = parseInput(input)
    val nodes = graph.keySet

    @tailrec def findAllGroups(remaining: Set[Int], groups: Int = 0): Int =
      if remaining.isEmpty then groups
      else
        val start = remaining.head
        val group = findConnectedGroup(graph, start)
        findAllGroups(remaining -- group, groups + 1)

    findAllGroups(nodes)

  private def parseInput(input: Seq[String]): Map[Int, Set[Int]] =
    def parseLine(line: String): (Int, Set[Int]) =
      val parts = line.split(" <-> ")
      val source = parts(0).toInt
      val targets = parts(1).split(", ").map(_.toInt).toSet
      source -> targets

    input.map(parseLine).toMap

  private def findConnectedGroup(graph: Map[Int, Set[Int]], start: Int): Set[Int] =
    def explore(current: Int, visited: Set[Int]): Set[Int] =
      if visited(current) then visited
      else
        val newVisited = visited + current
        graph(current).foldLeft(newVisited)((acc, neighbor) => explore(neighbor, acc))

    explore(start, Set.empty)
end Day12