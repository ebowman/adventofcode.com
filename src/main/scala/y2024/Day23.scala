package y2024

import scala.annotation.tailrec

// see https://adventofcode.com/2024/day/23
class Day23 extends util.Day(23):

  def solvePart1(input: IndexedSeq[String]): Int =
    val graph = buildGraph(input)
    val triplets = graph.findTriplets
    triplets.count(_.exists(_.startsWith("t")))
  end solvePart1

  def solvePart2(input: IndexedSeq[String]): String =

    val graph = buildGraph(input)

    val initialCandidates: List[Set[String]] =
      graph.adjacencyMap.keys.map: node =>
        graph.adjacencyMap(node) + node
      .toList

    val initialQueue: List[(Int, Set[String])] =
      initialCandidates.map: set =>
        (-set.size, set)
      .sortBy(_._1)

    graph.processQueue(initialQueue)
      .map(_.toSeq.sorted.mkString(","))
      .getOrElse(sys.error("No solution found"))

  end solvePart2

  private def buildGraph(input: IndexedSeq[String]): Graph =
    input.foldLeft(Graph(Map.empty)):
      case (graph: Graph, line: String) =>
        val Array(a, b) = line.split("-")
        Graph(
          graph.adjacencyMap
            .updatedWith(a)(neighbors => Some(neighbors.getOrElse(Set.empty) + b))
            .updatedWith(b)(neighbors => Some(neighbors.getOrElse(Set.empty) + a))
        )
  end buildGraph

  private case class Graph(adjacencyMap: Map[String, Set[String]]):
    def findTriplets: Set[Set[String]] =
      for
        (node, neighbors) <- adjacencyMap.toSet
        sortedNeighbors = neighbors.toSeq.sorted
        i <- sortedNeighbors.indices
        j <- (i + 1) until sortedNeighbors.size
        n1 = sortedNeighbors(i)
        n2 = sortedNeighbors(j)
        if adjacencyMap(n1).contains(n2)
      yield Set(node, n1, n2)
    end findTriplets

    def processQueue(initial: List[(Int, Set[String])]): Option[Set[String]] =
      processQueue(QueueState(initial, Set.empty))

    private def allConnected(nodes: Set[String]): Boolean =
      nodes.toSeq.combinations(2).forall:
        case Seq(a, b) =>
          adjacencyMap(a).contains(b)

    @tailrec
    private def processQueue(state: QueueState): Option[Set[String]] =
      state.queue match
        case Nil => None
        case (_, nodes) :: rest if state.seen.contains(nodes) =>
          processQueue(state.copy(queue = rest))
        case (negSize, nodes) :: rest =>
          if allConnected(nodes) then
            Some(nodes)
          else
            val newQueue = rest ++ nodes.toList.map: n =>
              val smaller = nodes - n
              if smaller.nonEmpty then
                (negSize + 1, smaller)
              else
                (negSize, smaller)

            processQueue(QueueState(newQueue, state.seen + nodes))
    end processQueue

    private case class QueueState(queue: List[(Int, Set[String])], seen: Set[Set[String]])
  end Graph

end Day23
