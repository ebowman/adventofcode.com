package y2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day03Spec extends AnyFlatSpec with Matchers with Day03 {

  lazy val input: IndexedSeq[String] = Loader(this, "day03.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day03.test.txt").toIndexedSeq

  it should "iterate in" in {
    val pos1 = neighborIterator(0, 0, 2, 5, 5).toSet
    pos1 shouldBe Set(Position(2,0), Position(0,1), Position(1,1), Position(2,1))

    val pos2 = neighborIterator(1, 1, 3, 5, 5).toSet
    pos2 shouldBe Set(
      Position(0,0), Position(1,0), Position(2,0), Position(3,0), Position(4, 0),
      Position(0,2), Position(1,2), Position(2,2), Position(3,2), Position(4, 2),
      Position(0,1), Position(4, 1))
  }

  it should "solve part 1 test" in {
    solvePart1(testInput) shouldBe 4361
  }

  it should "solve part 1" in {
    solvePart1(input) shouldBe 530495
  }

  it should "solve part 2 test" in {
    solvePart2(testInput) shouldBe 467835
  }

  it should "solve part 2" in {
    solvePart2(input) shouldBe 80253814
  }
}
