package y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day25Spec extends AnyFlatSpec with Matchers with Day25 {

  lazy val testInput: IndexedSeq[String] = Loader.is(this, "day25.test.txt")
  lazy val input: IndexedSeq[String] = Loader.is(this, "day25.txt")

  it should "solve part 1 test" in {
    part1(testInput) shouldBe 14897079
  }

  it should "solve part 1" in {
    part1(input) shouldBe 6408263
  }
}
