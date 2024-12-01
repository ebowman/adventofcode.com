package y2023

import scala.util.Random
import scala.collection.mutable
import scala.annotation.tailrec

class Day25 extends util.Day(25):
  case class Edge(a: String, b: String):
    def contains(component: String): Boolean = a == component || b == component
    def other(component: String): String = if a == component then b else a

  object Edge:
    def apply(a: String, b: String): Edge =
      if a.compareTo(b) <= 0 then new Edge(a, b) else new Edge(b, a)

  private def parseInput(input: Seq[String]): (Set[String], Set[Edge]) =
    val graph = mutable.Map[String, mutable.Set[String]]()

    input.foreach: line =>
      val parts = line.split(": ")
      val component = parts(0)
      val connected = parts(1).split(" ")

      graph.getOrElseUpdate(component, mutable.Set()) ++= connected
      connected.foreach: conn =>
        graph.getOrElseUpdate(conn, mutable.Set()) += component

    val vertices = graph.keySet.toSet
    val edges = graph.flatMap:
      case (comp, connected) =>
        connected.map(conn => Edge(comp, conn))
    .toSet

    (vertices, edges)

  private def kargerMinCut(vertices: Set[String], edges: Set[Edge]): (Int, Set[Edge]) =
    val random = new Random()
    val parent = mutable.Map[String, String]()
    val rank = mutable.Map[String, Int]()

    def find(x: String): String =
      if !parent.contains(x) then
        parent(x) = x
        rank(x) = 0
      if parent(x) != x then
        parent(x) = find(parent(x))
      parent(x)

    def union(x: String, y: String): Unit =
      val px = find(x)
      val py = find(y)
      if px != py then
        if rank(px) < rank(py) then
          parent(px) = py
        else if rank(px) > rank(py) then
          parent(py) = px
        else
          parent(py) = px
          rank(px) += 1

    // Initialize union-find
    vertices.foreach(v => find(v))

    // Keep track of remaining edges
    val remainingEdges = edges.toBuffer
    var groupCount = vertices.size

    // Contract until only two groups remain
    while groupCount > 2 do
      val idx = random.nextInt(remainingEdges.size)
      val edge = remainingEdges(idx)

      val group1 = find(edge.a)
      val group2 = find(edge.b)

      if group1 != group2 then
        union(group1, group2)
        groupCount -= 1

      remainingEdges.remove(idx)

    // Count edges between the two remaining groups
    val cutEdges = edges.filter: edge =>
      find(edge.a) != find(edge.b)

    // Count vertices in each group
    val groups = vertices.groupBy(find)
    val sizes = groups.values.map(_.size).toList

    (sizes.product, cutEdges)

  @tailrec
  private def findValidCut(vertices: Set[String], edges: Set[Edge], maxAttempts: Int = 1000): Option[(Int, Set[Edge])] =
    if maxAttempts <= 0 then None
    else
      val result = kargerMinCut(vertices, edges)
      if result._2.size == 3 then Some(result)
      else findValidCut(vertices, edges, maxAttempts - 1)

  def solvePart1(input: IndexedSeq[String]): Any =
    val (vertices, edges) = parseInput(input)
    findValidCut(vertices, edges) match
      case Some((product, _)) => product
      case None => throw new RuntimeException("Failed to find a valid cut after maximum attempts")
      
  def solvePart2(input: IndexedSeq[String]): Any = ???