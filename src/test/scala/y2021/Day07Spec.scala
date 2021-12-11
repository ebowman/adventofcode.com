package y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day07Spec extends AnyFlatSpec with Matchers with Day07 {
  lazy val testInput = "16,1,2,0,4,2,7,1,2,14"
  lazy val input = Loader(this, "day07.txt").toSeq.head

  "Day07" should "pass the part 1 tests" in {
    solve1(testInput) shouldBe 37
  }

  it should "pass part 1" in {
    solve1(input) shouldBe 344535
  }

  it should "pass part 2 test" in {
    solve2(testInput) shouldBe 168
  }

  it should "pass part 2" in {
    solve2(input) shouldBe 95581659
  }
}
