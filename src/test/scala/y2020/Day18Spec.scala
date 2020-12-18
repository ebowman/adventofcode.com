package y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day18Spec extends AnyFlatSpec with Matchers with Day18 {
  it should "solve part 1 test" in {
    val p = new Part1Parser {}
    p.parseAll(p.expr, "7").get shouldBe 7
    p.parseAll(p.expr, "1 + 2").get shouldBe 3
    p.parseAll(p.expr, "1 * 2").get shouldBe 2
    p.parseAll(p.expr, "1 + 2 * 3").get shouldBe 9
    p.parseAll(p.expr, "(6)").get shouldBe 6
    p.parseAll(p.expr, "(6+7)").get shouldBe 13
    p.parseAll(p.expr, "1 + (2 * 3)").get shouldBe 7
    p.parseAll(p.expr, "1 + 2 * 3 + 4 * 5 + 6").get shouldBe 71
    p.parseAll(p.expr, "1 + (2 * 3) + (4 * (5 + 6))").get shouldBe 51
    p.parseAll(p.expr, "2 * 3 + (4 * 5)").get shouldBe 26
    p.parseAll(p.expr, "5 + (8 * 3 + 9 + 3 * 4 * 3)").get shouldBe 437
    p.parseAll(p.expr, "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))").get shouldBe 12240
    p.parseAll(p.expr, "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2").get shouldBe 13632
  }

  it should "solve part 1" in {
    part1(Loader(this, "day18.txt").toIndexedSeq) shouldBe 31142189909908L
  }

  it should "solve part 2 test" in {
    val p = new Part2Parser {}
    p.parseAll(p.expr, "7").get shouldBe 7
    p.parseAll(p.expr, "1 + 2").get shouldBe 3
    p.parseAll(p.expr, "1 * 2").get shouldBe 2
    p.parseAll(p.expr, "1 + 2 * 3").get shouldBe 9
    p.parseAll(p.expr, "1 * 2 + 3").get shouldBe 5
    p.parseAll(p.expr, "( 6 )").get shouldBe 6
    p.parseAll(p.expr, "( 6 + 7 )").get shouldBe 13
    p.parseAll(p.expr, "1 + ( 2 * 3 )").get shouldBe 7
    p.parseAll(p.expr, "1 + 2 * 3 + 4 * 5 + 6").get shouldBe 231
    p.parseAll(p.expr, "1 + ( 2 * 3 ) + ( 4 * ( 5 + 6 ) )").get shouldBe 51
    p.parseAll(p.expr, "2 * 3 + (4 * 5)").get shouldBe 46
    p.parseAll(p.expr, "5 + (8 * 3 + 9 + 3 * 4 * 3)").get shouldBe 1445
    p.parseAll(p.expr, "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))").get shouldBe 669060
    p.parseAll(p.expr, "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2").get shouldBe 23340
  }

  it should "solve part 2" in {
    part2(Loader(this, "day18.txt").toIndexedSeq) shouldBe 323912478287549L
  }
}
