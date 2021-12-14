package y2021

import scala.annotation.tailrec
import scala.collection.mutable

trait Day14 {

  def loadRules(input: Seq[String]): (String, Map[String, String]) = {
    val seed = input.head
    val rules = input.tail.tail.map { rule =>
      val Array(left, right) = rule.split(" -> ")
      left -> right
    }.toMap
    (seed, rules)
  }

  def solve1(input: Seq[String], count: Int): Int = {
    @tailrec def iter(rules: Map[String, String], seed: String, count: Int): String = {
      if (count == 0) seed
      else {
        val sb = new StringBuilder
        for ((cur, next) <- seed.zip(seed.tail)) {
          val rule = s"$cur$next"
          if (rules.contains(rule)) sb.append(s"$cur${rules(rule)}")
          else sb.append(cur)
        }
        sb.append(seed.last)
        iter(rules, sb.toString(), count - 1)
      }
    }

    val (seed, rules) = loadRules(input)
    val end = iter(rules, seed, count)
    val sorted = end.groupBy(x => x).toSeq.sortBy(_._2.length)
    sorted.last._2.length - sorted.head._2.length
  }

  def solve2(input: Seq[String], count: Int): Long = {
    type RMap = mutable.Map[String, Long]

    def newMap(): RMap = new collection.mutable.HashMap[String, Long]().withDefaultValue(0L)

    val (seed, rules) = loadRules(input)

    @tailrec def compute(tupleFrequencyCounts: RMap, count: Int): RMap = {
      if (count == 0) tupleFrequencyCounts
      else {
        val newFrequencies = {
          val newM: RMap = newMap()
          tupleFrequencyCounts.foreach { case (k, v) =>
            newM(k(0) + rules(k)) += v
            newM(rules(k) + k(1)) += v
          }
          newM
        }
        compute(newFrequencies, count - 1)
      }
    }

    val pairFrequencyCounts = {
      val fc = newMap()
      seed.sliding(2).foreach { r => fc(r) += 1 }
      fc
    }
    val finalPairFrequencyCounts = compute(pairFrequencyCounts, count)
    val perLetterCounts = {
      val plc = newMap()
      finalPairFrequencyCounts.keys.foreach { letter =>
        plc(s"${letter.head}") += finalPairFrequencyCounts(letter)
      }
      plc(s"${seed.last}") += 1
      plc
    }
    perLetterCounts.maxBy(_._2)._2 - perLetterCounts.minBy(_._2)._2
  }
}