
package y2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*
import util.Loader

class Day01Spec extends AnyFlatSpec with Matchers with Day01 {

  lazy val input: IndexedSeq[String] = Loader(this, "day01.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day01.test.txt").toIndexedSeq
  lazy val testInput2: IndexedSeq[String] = Loader(this, "day01.test2.txt").toIndexedSeq

  it should "solve part 1 test" in {
    val result = testInput.map(solve)
    result shouldBe IndexedSeq(12, 38, 15, 77)
    result.sum shouldBe 142
  }

  it should "solve part 1" in {
    input.map(solve).sum shouldBe 55538
  }

  it should "solve part 2 test" in {
    val result = testInput2.map(solve2)
    result shouldBe IndexedSeq(29, 83, 13, 24, 42, 14, 76)
    result.sum shouldBe 281
  }

  it should "solve part 2" in {
    input.map(solve2).sum shouldBe 54875
  }
}
