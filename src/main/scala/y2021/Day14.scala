package y2021

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
    def map() = Map[String, Long]().withDefaultValue(0L)

    val (seed, rules) = loadRules(input)
    val perLetterCounts = {
      val plc = (1 to count).foldLeft {
        seed.sliding(2).foldLeft(map()) { case (fc, r) =>
          fc + (r -> (fc(r) + 1))
        }
      } { case (tfc, _) =>
        tfc.foldLeft(map()) { case (map, (k, v)) =>
          val k1 = k(0) + rules(k)
          val k2 = rules(k) + k(1)
          map + (k1 -> (map(k1) + v)) + (k2 -> (map(k2) + v))
        }
      }.foldLeft(map()) { case (plc, (letter, value)) =>
        val k = s"${letter.head}"
        plc + (k -> (plc(k) + value))
      }
      plc + (s"${seed.last}" -> (plc(s"${seed.last}") + 1))
    }
    perLetterCounts.maxBy(_._2)._2 - perLetterCounts.minBy(_._2)._2
  }
}
