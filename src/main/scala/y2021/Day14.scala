package y2021

import scala.annotation.tailrec

trait Day14 {

  def loadRules(input: Seq[String]): (String, Map[String, String]) = {
    val seed = input.head
    val rules = input.tail.tail.map { rule =>
      val Array(left, right) = rule.split(" -> ")
      left -> right
    }.toMap
    (seed, rules)
  }

  def solve(input: Seq[String], count: Int): Long = {
    type CMap = Map[String, Long]

    val (seed, rules) = loadRules(input)

    @tailrec def compute(tupleFrequencyCounts: CMap, count: Int): CMap = {
      if (count == 0) tupleFrequencyCounts
      else {
        def recurFreq(c: CMap, f: CMap = Map().withDefaultValue(0L)): CMap = {
          if (c.isEmpty) f
          else {
            val (k, v) = c.head
            val k1 = k(0) + rules(k)
            val k2 = rules(k) + k(1)
            recurFreq(c.tail, f + (k1 -> (f(k1) + v)) + (k2 -> (f(k2) + v)))
          }
        }
        compute(recurFreq(tupleFrequencyCounts), count - 1)
      }
    }

    @tailrec def slideFreq(pairs: Iterator[String], fc: CMap = Map().withDefaultValue(0L)): CMap = {
      if (pairs.isEmpty) fc
      else {
        val r = pairs.next()
        slideFreq(pairs, fc + (r -> (fc(r) + 1L)))
      }
    }

    val pairFrequencyCounts = slideFreq(seed.sliding(2))
    val finalPairFrequencyCounts = compute(pairFrequencyCounts, count)

    @tailrec def recurseCount(keys: Iterable[String], plc: CMap = Map().withDefaultValue(0L)): CMap = {
      if (keys.isEmpty) plc + (s"${seed.last}" -> (plc(s"${seed.last}") + 1))
      else {
        val letter = keys.head
        val k = s"${letter.head}"
        recurseCount(keys.tail, plc + (k -> (plc(k) + finalPairFrequencyCounts(letter))))
      }
    }

    val perLetterCounts = recurseCount(finalPairFrequencyCounts.keys)
    perLetterCounts.maxBy(_._2)._2 - perLetterCounts.minBy(_._2)._2
  }
}