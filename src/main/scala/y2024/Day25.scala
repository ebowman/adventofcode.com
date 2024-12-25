package y2024

class Day25 extends util.Day(25):

  def solvePart1(input: IndexedSeq[String]): Int =
    val (locks, keys) = Pattern.parseInput(input).partition(_.isLock)
    Pattern.countValidPairs(locks, keys)
  end solvePart1

  private case class Pattern(heights: List[Int], raw: List[String]):
    def isLock: Boolean = raw.head.contains('#') && raw.last.forall(_ == '.')

  private object Pattern:
    def parseInput(input: IndexedSeq[String]): List[Pattern] =
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
    end parseInput

    private def isValidPair(lock: Pattern, key: Pattern): Boolean =
      lock.heights.zip(key.heights).forall: (l, k) =>
        l + k <= 7

    def countValidPairs(locks: List[Pattern], keys: List[Pattern]): Int =
      locks
        .cross(keys)
        .count(isValidPair.tupled)

    extension [A](list: List[A])
      private def cross[B](other: List[B]): List[(A, B)] =
        list.flatMap(a => other.map(b => (a, b)))
  end Pattern

  def solvePart2(input: IndexedSeq[String]): String = ???

end Day25
