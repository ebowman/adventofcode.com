package y2024

// see https://adventofcode.com/2024/day/19
class Day19 extends util.Day(19):

  def solvePart1(input: IndexedSeq[String]): Int =
    val (patterns, designs) = parseInput(input)
    designs.count(design => countConstruct(design, patterns) > 0)

  def solvePart2(input: IndexedSeq[String]): Long =
    val (patterns, designs) = parseInput(input)
    designs.map(design => countConstruct(design, patterns)).sum

  private def parseInput(input: IndexedSeq[String]): (Set[String], List[String]) =
    input.toList match
      case patternsLine :: "" :: rest =>
        val patterns = patternsLine.split(",").map(_.trim).toSet
        (patterns, rest)
      case _ =>
        throw new IllegalArgumentException("Invalid input format.")

  private def countConstruct(design: String, patterns: Set[String]): Long =
    def recurse(index: Int, memo: Map[Int, Long]): (Long, Map[Int, Long]) =
      if index == design.length then (1L, memo)
      else
        memo.get(index) match
          case Some(count) => (count, memo)
          case None =>
            val (total, updatedMemo) = patterns.foldLeft((0L, memo)):
              case ((acc, currentMemo), pattern) =>
                val end = index + pattern.length
                if end <= design.length && design.startsWith(pattern, index) then
                  val (count, newMemo) = recurse(end, currentMemo)
                  (acc + count, newMemo)
                else
                  (acc, currentMemo)
            (total, updatedMemo + (index -> total))

    recurse(0, Map.empty)._1
end Day19
