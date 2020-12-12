package y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day10Spec extends AnyFlatSpec with Matchers with Day10 {
  lazy val testData: IndexedSeq[Int] =
    """
      |16
      |10
      |15
      |5
      |1
      |11
      |7
      |19
      |6
      |12
      |4""".stripMargin.trim.linesIterator.toIndexedSeq.map(_.toInt)

  lazy val testData2: IndexedSeq[Int] =
    """ |
      |28
      |33
      |18
      |42
      |31
      |14
      |46
      |20
      |48
      |47
      |24
      |23
      |49
      |45
      |19
      |38
      |39
      |11
      |1
      |32
      |25
      |35
      |8
      |17
      |7
      |9
      |4
      |2
      |34
      |10
      |3""".stripMargin.trim.linesIterator.toIndexedSeq.map(_.toInt)


  lazy val data: IndexedSeq[Int] = Loader(this, "day10.txt").map(_.toInt).toIndexedSeq

  it should "pass part 1 test 1" in {
    solve(testData) shouldBe 35
  }

  it should "pass part 1 test 2" in {
    solve(testData2) shouldBe 220
  }

  it should "pass part 1" in {
    solve(data) shouldBe 1820
  }

  it should "pass part 2 test 1" in {
    solve2(testData) shouldBe 8
  }

  it should "pass part 2 test 2" in {
    solve2(testData2) shouldBe 19208
  }

  it should "pass part 2" in {
    solve2(data) shouldBe 3454189699072L
  }
}
