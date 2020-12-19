package y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day19Spec extends AnyFlatSpec with Matchers with Day19 {
  it should "parse" in {
    parseAll(ruleParser, "4: \"a\"").get shouldBe CoreRule(4, "a")
    parseAll(ruleParser, "0: 4 1 5").get shouldBe SeqRule(0, Seq(4, 1, 5))
    parseAll(ruleParser, "3: 4 5 | 5 4").get shouldBe OrRule(3, Seq(4, 5), Seq(5, 4))

    val input =
      """
        |0: 4 1 5
        |1: 2 3 | 3 2
        |2: 4 4 | 5 5
        |3: 4 5 | 5 4
        |4: "a"
        |5: "b"""".stripMargin.trim.linesIterator.toIndexedSeq
    val dict = parseDict(input)
    dict(5).build(dict) shouldBe "b"
    dict(4).build(dict) shouldBe "a"
    dict(3).build(dict) shouldBe "((ab)|(ba))"
    dict(1).build(dict) shouldBe "((((aa)|(bb))((ab)|(ba)))|(((ab)|(ba))((aa)|(bb))))"
  }

  it should "solve part 1 test" in {
    enablePart2 = false
    part1(
      """
        |0: 4 1 5
        |1: 2 3 | 3 2
        |2: 4 4 | 5 5
        |3: 4 5 | 5 4
        |4: "a"
        |5: "b"
        |
        |ababbb
        |bababa
        |abbbab
        |aaabbb
        |aaaabbb""".stripMargin.trim.linesIterator.toIndexedSeq) shouldBe 2
  }

  it should "solve part 1" in {
    enablePart2 = false
    part1(Loader(this, "day19.txt").toIndexedSeq) shouldBe 139
  }

  it should "solve part 2 test" in {
    val oldInput =
      """
        |42: 9 14 | 10 1
        |9: 14 27 | 1 26
        |10: 23 14 | 28 1
        |1: "a"
        |11: 42 31
        |5: 1 14 | 15 1
        |19: 14 1 | 14 14
        |12: 24 14 | 19 1
        |16: 15 1 | 14 14
        |31: 14 17 | 1 13
        |6: 14 14 | 1 14
        |2: 1 24 | 14 4
        |0: 8 11
        |13: 14 3 | 1 12
        |15: 1 | 14
        |17: 14 2 | 1 7
        |23: 25 1 | 22 14
        |28: 16 1
        |4: 1 1
        |20: 14 14 | 1 15
        |3: 5 14 | 16 1
        |27: 1 6 | 14 18
        |14: "b"
        |21: 14 1 | 1 14
        |25: 1 1 | 1 14
        |22: 14 14
        |8: 42
        |26: 14 22 | 1 20
        |18: 15 15
        |7: 14 5 | 1 21
        |24: 14 1
        |
        |abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
        |bbabbbbaabaabba
        |babbbbaabbbbbabbbbbbaabaaabaaa
        |aaabbbbbbaaaabaababaabababbabaaabbababababaaa
        |bbbbbbbaaaabbbbaaabbabaaa
        |bbbababbbbaaaaaaaabbababaaababaabab
        |ababaaaaaabaaab
        |ababaaaaabbbaba
        |baabbaaaabbaaaababbaababb
        |abbbbabbbbaaaababbbbbbaaaababb
        |aaaaabbaabaaaaababaa
        |aaaabbaaaabbaaa
        |aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
        |babaaabbbaaabaababbaabababaaab
        |aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba""".stripMargin.trim.linesIterator.toIndexedSeq

    enablePart2 = false
    part1(oldInput) shouldBe 3
    enablePart2 = true
    part1(oldInput) shouldBe 12
  }

  it should "solve part 2" in {
    enablePart2 = true
    part1(Loader(this, "day19.txt").toIndexedSeq) shouldBe 289
  }
}
