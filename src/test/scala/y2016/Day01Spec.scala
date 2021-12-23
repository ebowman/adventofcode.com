package y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day01Spec extends AnyFlatSpec with Matchers with Day01 {
  private lazy val input = util.Loader(this, "day01.txt").toSeq

  it should "pass part 1 tests" in {
    solve1(Seq("R2", "L3")) shouldBe 5
    solve1(Seq("R2", "R2", "R2")) shouldBe 2
    solve1(Seq("R5", "L5", "R5", "R3")) shouldBe 12
  }

  it should "pass part 1" in {
    solve1(input.head.split(",").map(_.trim).toSeq) shouldBe 234
  }

  it should "pass part 2 tests" in {
    solve2(Seq("R8", "R4", "R4", "R8")) shouldBe 4
  }

  it should "pass part 2" in {
    solve2(input.head.split(",").map(_.trim).toSeq) shouldBe 113
  }
}
