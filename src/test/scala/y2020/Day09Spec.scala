package y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day09Spec extends AnyFlatSpec with Matchers with Day09 {
  lazy val testData: IndexedSeq[Long] =
    """
      |35
      |20
      |15
      |25
      |47
      |40
      |62
      |55
      |65
      |95
      |102
      |117
      |150
      |182
      |127
      |219
      |299
      |277
      |309
      |576
      |""".stripMargin.trim.linesIterator.toIndexedSeq.map(_.toLong)

  lazy val liveData: IndexedSeq[Long] = Loader(this, "day09.txt").map(_.toLong).toIndexedSeq

  it should "pass part 1 test" in {
    part1(testData, 5, testData.drop(5)) shouldBe 127
  }

  it should "pass part 1" in {
    part1(liveData, 25, liveData.drop(25)) shouldBe 466456641
  }

  it should "pass part 2 test" in {
    part2(testData, 127) shouldBe 62
  }

  it should "pass part 2" in {
    var i = 0
    while (i < 10) {
      part2(liveData, 466456641) shouldBe 55732936
      i += 1
    }
  }
}
