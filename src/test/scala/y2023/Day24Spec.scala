package y2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day24Spec extends AnyFlatSpec with Matchers with Day24 {

  lazy val input: IndexedSeq[String] = Loader(this, "day24.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day24.test.txt").toIndexedSeq

  it should "solve part 1 test" in {
    solvePart1(testInput, 9.0, 25.0) shouldBe 2
  }

  it should "solve part 1" in {
    solvePart1(input, 200000000000000.0, 400000000000000.0) shouldBe 31921
  }

  it should "solve part 2 test" in {
    solvePart2(testInput) shouldBe 47
  }

  it should "solve part 2" in {
    solvePart2(input) shouldBe 761691907059631L
  }
}
