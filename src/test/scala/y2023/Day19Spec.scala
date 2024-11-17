package y2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day19Spec extends AnyFlatSpec with Matchers with Day19 {

  lazy val input: IndexedSeq[String] = Loader(this, "day19.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day19.test.txt").toIndexedSeq

  it should "solve part 1 test" in {
    solvePart1(testInput) shouldBe 19114
  }

  it should "solve part 1" in {
    solvePart1(input) shouldBe 492702
  }

  it should "solve part 2 test" in {
    solvePart2(testInput) shouldBe 167409079868000L
  }

  it should "solve part 2" in {
    solvePart2(input) shouldBe 138616621185978L
  }
}
