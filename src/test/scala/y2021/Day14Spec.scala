package y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day14Spec extends AnyFlatSpec with Matchers with Day14 {
  lazy val testInput = Loader(this, "day14.test.txt").toSeq
  lazy val input = Loader(this, "day14.txt").toSeq

  "Day14" should "pass the part 1 tests" in {
    solve1(testInput, 10) shouldBe 1588
  }

  it should "pass part 1" in {
    solve1(input, 10) shouldBe 2010
  }

  it should "pass part 2 test" in {
    solve2(testInput, count = 40) shouldBe 2188189693529L
  }

  it should "pass part 2" in {
    solve2(input, count = 40) shouldBe 2437698971143L
  }
}
