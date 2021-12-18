package y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day18Spec extends AnyFlatSpec with Matchers with Day18 {
  private lazy val testInput = util.Loader(this, "day18.test.txt").toSeq
  private lazy val input = util.Loader(this, "day18.txt").toSeq

  it should "parse" in {
    parse("[2,3]") shouldBe Seq(Num(2, 1), Num(3, 1))
    parse("[[1,2],[[3,4],5]]") shouldBe Seq(Num(1, 2), Num(2, 2), Num(3, 3), Num(4, 3), Num(5, 2))
    parse("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]") shouldBe
      Seq(Num(8, 4), Num(7, 4), Num(7, 4), Num(7, 4), Num(8, 4), Num(6, 4), Num(7, 4), Num(7, 4), Num(0, 4),
        Num(7, 4), Num(6, 4), Num(6, 4), Num(8, 3), Num(7, 3))
  }

  it should "correctly explode" in {
    parse("[[[[[9,8],1],2],3],4]").explode shouldBe parse("[[[[0,9],2],3],4]")
    parse("[7,[6,[5,[4,[3,2]]]]]").explode shouldBe parse("[7,[6,[5,[7,0]]]]")
    parse("[[6,[5,[4,[3,2]]]],1]").explode shouldBe parse("[[6,[5,[7,0]]],3]")
    parse("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]").explode shouldBe parse("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
    parse("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]").explode shouldBe parse("[[3,[2,[8,0]]],[9,[5,[7,0]]]]")
  }

  it should "correctly split" in {
    parse("10").split shouldBe parse("[5,5]")
    parse("11").split shouldBe parse("[5,6]")
    parse("[[[[0,7],4],[15,[0,13]]],[1,1]]").split shouldBe parse("[[[[0,7],4],[[7,8],[0,13]]],[1,1]]")
    parse("[[[[0,7],4],[[7,8],[0,13]]],[1,1]]").split shouldBe parse("[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]")
  }

  it should "correctly add" in {
    parse("[[[[4,3],4],4],[7,[[8,4],9]]]") |+| parse("[1,1]") shouldBe
      parse("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")
  }

  it should "correctly compute magnitudes" in {
    parse("[[1,2],[[3,4],5]]").magnitude shouldBe 143
    parse("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]").magnitude shouldBe 1384
    parse("[[[[1,1],[2,2]],[3,3]],[4,4]]").magnitude shouldBe 445
    parse("[[[[3,0],[5,3]],[4,4]],[5,5]]").magnitude shouldBe 791
    parse("[[[[5,0],[7,4]],[5,5]],[6,6]]").magnitude shouldBe 1137
    parse("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]").magnitude shouldBe 3488
  }

  it should "add numbers correctly" in {
    val tmp = Seq("[1,1]", "[2,2]", "[3,3]", "[4,4]").map(str => parse(str))
    tmp.tail.foldLeft(tmp.head) { case (a, b) => a |+| b } shouldBe parse("[[[[1,1],[2,2]],[3,3]],[4,4]]")
  }

  it should "pass part 1 tests" in {
    solve1(testInput) shouldBe 4140
  }

  it should "pass part 1" in {
    solve1(input) shouldBe 4323
  }

  it should "pass part 2 tests" in {
    solve2(testInput) shouldBe 3993
  }
  it should "pass part 2" in {
    solve2(input) shouldBe 4749
  }
}
