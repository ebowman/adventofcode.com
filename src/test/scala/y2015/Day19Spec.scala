package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day19Spec extends AnyFlatSpec with Matchers with Day19 {

  "Parser" should "parse the test data" in {
    val input =
      """H => HO
        |H => OH
        |O => HH""".stripMargin.trim.linesIterator.iterator.to(Iterable)

    input.map(line => parseAll(rule, line).get) shouldBe Seq(("H", "HO"), ("H", "OH"), ("O", "HH"))
  }

  it should "parse the input file" in {
    val input =
      """H => HO
        |H => OH
        |O => HH
        |
        |HOH""".stripMargin.trim.linesIterator.iterator.to(Iterable)

    val sys = mkSystem(input)
    sys.next.size shouldBe 4
  }

  it should "solve the first part of the puzzle" in {
    val sys = mkSystem(Loader(this, "day19.txt"))
    sys.next.size shouldBe 535
  }

  it should "solve the sample case in #2" in {
    val input =
      """e => H
        |e => O
        |H => HO
        |H => OH
        |O => HH
        |
        |HOH""".stripMargin.trim.linesIterator.iterator.to(Iterable)
    val target = mkSystem(input)
    solveWide(target.clear, target.molecule) shouldBe 3
    solveDeep(target.clear, target.molecule) shouldBe 3
  }

  it should "solve the second sample case in #2" in {
    val input =
      """e => H
        |e => O
        |H => HO
        |H => OH
        |O => HH
        |
        |HOHOHO""".stripMargin.trim.linesIterator.iterator.to(Iterable)
    val target = mkSystem(input)
    solveWide(target.clear, target.molecule) shouldBe 6
    solveDeep(target.clear, target.molecule) shouldBe 6
  }

  it should "solve the final case" in {
    val sys = mkSystem(Loader(this, "day19.txt"))
    solveSneaky(sys.clear, sys.molecule) shouldBe 212
  }

}

