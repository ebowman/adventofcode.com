package y2024

import scala.annotation.tailrec

class Day11 extends util.Day(11):
  
  def solvePart1(input: IndexedSeq[String]): Long =
    val initial = input.head.split(" ").map(_.toLong).toSeq
    countAfterSteps(initial, 25)

  def solvePart2(input: IndexedSeq[String]): Long =
    val initial = input.head.split(" ").map(_.toLong).toSeq
    countAfterSteps(initial, 75)

  private def blinkOne(stone: Long): Seq[Long] =
    if stone == 0 then Seq(1L)
    else
      val stoneStr = stone.toString
      val length = stoneStr.length
      if length % 2 == 0 then
        val mid = length / 2
        Seq(stoneStr.take(mid).toLong, stoneStr.drop(mid).toLong)
      else
        Seq(stone * 2024L)
  end blinkOne

  private def blinkAll(stoneCounts: Map[Long, Long]): Map[Long, Long] =
    stoneCounts.toSeq.foldLeft(Map.empty[Long, Long].withDefaultValue(0L)):
      case (newCounts, (stone, count)) =>
        blinkOne(stone).foldLeft(newCounts):
          case (counts, newStone) =>
            counts + (newStone -> (counts(newStone) + count))
  end blinkAll

  private def countAfterSteps(initial: Seq[Long], steps: Int): Long =
    @tailrec
    def recurse(counts: Map[Long, Long], remainingSteps: Int): Long =
      if remainingSteps == 0 then
        counts.values.sum
      else
        recurse(blinkAll(counts), remainingSteps - 1)

    val initialCounts = initial.groupBy(identity).view.mapValues(_.size.toLong).toMap
    recurse(initialCounts, steps)
    
end Day11
