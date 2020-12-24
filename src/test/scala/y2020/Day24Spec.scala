package y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day24Spec extends AnyFlatSpec with Matchers with Day24 {

  lazy val testInput: IndexedSeq[String] = Loader.is(this, "day24.test.txt")
  lazy val input: IndexedSeq[String] = Loader.is(this, "day24.txt")

  it should "solve part 1 test" in {
    part1(testInput) shouldBe 10
  }

  it should "solve part 1" in {
    part1(input) shouldBe 427
  }

  it should "solve part 2 test" in {
    part2(testInput) shouldBe 2208
  }

  it should "solve part 2" in {
    part2(input) shouldBe 3837
  }
}
