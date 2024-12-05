package y2024

import scala.collection.immutable.Queue
import scala.collection.mutable

class Day05 extends util.Day(5):
  case class Rule(before: Int, after: Int)

  private def parseInput(input: IndexedSeq[String]): (Set[Rule], List[List[Int]]) =
    val (rulesSection, updatesSection) = input.span(_.contains("|"))

    val rules = rulesSection.map: line =>
      val Array(before, after) = line.split("""\|""")
      Rule(before.toInt, after.toInt)
    .toSet

    val updates = updatesSection
      .filterNot(_.isEmpty)
      .map: line =>
        line.split(",").map(_.toInt).toList
      .toList

    (rules, updates)
  end parseInput

  private def getApplicableRules(pages: Set[Int], rules: Set[Rule]): Set[Rule] =
    rules.filter: rule =>
      pages.contains(rule.before) && pages.contains(rule.after)

  private def isCorrectlyOrdered(update: List[Int], rules: Set[Rule]): Boolean =
    val positions = update.zipWithIndex.toMap
    getApplicableRules(update.toSet, rules).forall: rule =>
      positions(rule.before) < positions(rule.after)

  def solvePart1(input: IndexedSeq[String]): Int =
    val (rules, updates) = parseInput(input)
    updates
      .filter(update => isCorrectlyOrdered(update, rules))
      .map(update => update(update.length / 2))
      .sum

  private def topologicalSort(nodes: Set[Int], rules: Set[Rule]): List[Int] =
    val inDegree = mutable.Map[Int, Int]().withDefault(_ => 0)
    val graph = mutable.Map[Int, Set[Int]]().withDefault(_ => Set())

    rules.foreach: rule =>
      graph(rule.before) = graph(rule.before) + rule.after
      inDegree(rule.after) = inDegree(rule.after) + 1

    var queue = Queue.from:
      nodes.filter:
        inDegree(_) == 0

    var result = List[Int]()
    while queue.nonEmpty do
      val (current, newQueue) = queue.dequeue
      queue = newQueue
      result = result :+ current

      graph(current).foreach: next =>
        inDegree(next) -= 1
        if inDegree(next) == 0 then
          queue = queue.enqueue(next)

    result
  end topologicalSort

  def solvePart2(input: IndexedSeq[String]): Int =
    val (rules, updates) = parseInput(input)
    updates
      .filterNot(update => isCorrectlyOrdered(update, rules))
      .map: update =>
        val sortedUpdate = topologicalSort(update.toSet, getApplicableRules(update.toSet, rules))
        sortedUpdate(sortedUpdate.length / 2)
      .sum
end Day05