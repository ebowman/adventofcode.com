package y2023

import scala.annotation.tailrec
import scala.util.boundary
import scala.util.boundary.break

trait Day19 {
  case class Part(x: Int, m: Int, a: Int, s: Int):
    def getValue(category: String): Int = category match
      case "x" => x
      case "m" => m
      case "a" => a
      case "s" => s

    def sum: Int = x + m + a + s

  case class Rule(category: String = "", operator: String = "", value: Int = 0, destination: String):
    def matches(part: Part): Boolean =
      if category.isEmpty then true
      else
        val partValue = part.getValue(category)
        operator match
          case ">" => partValue > value
          case "<" => partValue < value
          case _ => false

  case class Workflow(name: String, rules: List[Rule])

  case class Range(min: Int, max: Int):
    def size: Long = if max < min then 0 else max - min + 1L

    def split(value: Int, lessThan: Boolean): (Option[Range], Option[Range]) =
      if lessThan then
        // For x < value
        val matchedRange = Option.when(min < value)(Range(min, value - 1))
        val remainingRange = Option.when(max >= value)(Range(value, max))
        (matchedRange, remainingRange)
      else
        // For x > value
        val matchedRange = Option.when(max > value)(Range(value + 1, max))
        val remainingRange = Option.when(min <= value)(Range(min, value))
        (matchedRange, remainingRange)

  case class PartRanges(x: Range, m: Range, a: Range, s: Range):
    def possibilities: Long = x.size * m.size * a.size * s.size

    def withCategory(category: String, range: Range): PartRanges = category match
      case "x" => copy(x = range)
      case "m" => copy(m = range)
      case "a" => copy(a = range)
      case "s" => copy(s = range)

    def getRange(category: String): Range = category match
      case "x" => x
      case "m" => m
      case "a" => a
      case "s" => s

  def parseRule(ruleStr: String): Rule =
    if !ruleStr.contains(":") then Rule(destination = ruleStr)
    else
      val Array(condition, destination) = ruleStr.split(":")
      val category = condition(0).toString
      val operator = condition(1).toString
      val value = condition.substring(2).toInt
      Rule(category, operator, value, destination)

  def parseWorkflow(line: String): Workflow =
    val name = line.takeWhile(_ != '{')
    val rulesStr = line.dropWhile(_ != '{').drop(1).dropRight(1)
    val rules = rulesStr.split(",").map(parseRule).toList
    Workflow(name, rules)

  def parsePart(line: String): Part =
    val nums = line.drop(1).dropRight(1).split(",").map(_.split("=")(1).toInt)
    Part(nums(0), nums(1), nums(2), nums(3))

  def processPart(part: Part, workflows: Map[String, Workflow], current: String = "in"): Boolean =
    boundary:

      @tailrec def process(current: String): Boolean =
        current match
          case "A" => break(true)
          case "R" => break(false)
          case _ =>
            val workflow = workflows(current)
            workflow.rules.find(_.matches(part)) match
              case Some(rule) => process(rule.destination)
              case None => break(false)

      process(current)

  def countAcceptedCombinations(
                                 ranges: PartRanges,
                                 workflows: Map[String, Workflow],
                                 current: String
                               ): Long =
    @tailrec def processRules(rules: List[Rule],
                              remainingRanges: PartRanges,
                              accTotal: Long): Long =
      rules match
        case Nil => accTotal
        case rule :: remaining =>
          if rule.category.isEmpty then
            accTotal + countAcceptedCombinations(remainingRanges, workflows, rule.destination)
          else
            val currentRange = remainingRanges.getRange(rule.category)
            val (matchedRange, unmatchedRange) = currentRange.split(
              rule.value,
              rule.operator == "<"
            )

            val matchedTotal = matchedRange.fold(0L) { range =>
              val newRanges = remainingRanges.withCategory(rule.category, range)
              countAcceptedCombinations(newRanges, workflows, rule.destination)
            }

            unmatchedRange match
              case Some(range) if range.size > 0 =>
                val newRemaining = remainingRanges.withCategory(rule.category, range)
                processRules(remaining, newRemaining, accTotal + matchedTotal)
              case _ => accTotal + matchedTotal

    current match
      case "A" => ranges.possibilities
      case "R" => 0L
      case _ if ranges.possibilities == 0 => 0L
      case _ =>
        val workflow = workflows(current)
        processRules(workflow.rules, ranges, 0L)

  def parsePreamble(input: Seq[String]): (Map[String, Workflow], Array[String]) =
    val sections = input.mkString("\n").trim.split("\n\n")
    val workflows = sections(0).split("\n")
      .map(parseWorkflow)
      .map(w => w.name -> w)
      .toMap
    (workflows, sections)

  def solvePart1(input: Seq[String]): Int =
    val (workflows, sections) = parsePreamble(input)
    val parts = sections(1).split("\n").map(parsePart)
    parts.filter(processPart(_, workflows))
      .map(_.sum)
      .sum

  def solvePart2(input: Seq[String]): Long =
    val (workflows, _) = parsePreamble(input)
    val initialRanges = PartRanges(
      x = Range(1, 4000),
      m = Range(1, 4000),
      a = Range(1, 4000),
      s = Range(1, 4000)
    )

    countAcceptedCombinations(initialRanges, workflows, "in")
}