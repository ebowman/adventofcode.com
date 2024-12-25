package y2024

class Day25 extends util.Day(25):
  case class Pattern(heights: List[Int], raw: List[String])

  def solvePart1(input: IndexedSeq[String]): Int =
    val patterns = parsePatterns(input)
    val locks = patterns.filter(isLock)
    val keys = patterns.filter(isKey)
    countValidPairs(locks, keys)
  end solvePart1

  def solvePart2(input: IndexedSeq[String]): String = ???

  private def parsePatterns(input: IndexedSeq[String]): List[Pattern] =
    input
      .filterNot(_.isEmpty)
      .grouped(7)
      .map: group =>
        val columns = group.transpose
        val heights = columns.map: col =>
          if col(0) == '#' then
            col.takeWhile(_ == '#').length
          else
            col.reverse.takeWhile(_ == '#').length
        .toList
        Pattern(heights, group.toList)
      .toList
  end parsePatterns

  private def isValidPair(lock: Pattern, key: Pattern): Boolean =
    lock.heights.zip(key.heights).forall: (l, k) =>
      l + k <= 7

  private def isLock(pattern: Pattern): Boolean =
    pattern.raw.head.contains('#') && pattern.raw.last.forall(_ == '.')

  private def isKey(pattern: Pattern): Boolean =
    pattern.raw.head.forall(_ == '.') && pattern.raw.last.contains('#')

  private def countValidPairs(locks: List[Pattern], keys: List[Pattern]): Int =
    locks.flatMap(lock => keys.map(key => isValidPair(lock, key))).count(identity)

end Day25
