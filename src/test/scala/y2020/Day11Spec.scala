package y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day11Spec extends AnyFlatSpec with Matchers with Day11 {
  lazy val testData: IndexedSeq[String] =
    """
      |L.LL.LL.LL
      |LLLLLLL.LL
      |L.L.L..L..
      |LLLL.LL.LL
      |L.LL.LL.LL
      |L.LLLLL.LL
      |..L.L.....
      |LLLLLLLLLL
      |L.LLLLLL.L
      |L.LLLLL.LL""".stripMargin.trim.linesIterator.toIndexedSeq

  it should "solve part 1 test" in {
    Seats(testData).solve1() shouldBe 37
  }

  it should "solve part 1" in {
    Seats(Loader(this, "day11.txt")).solve1() shouldBe 2424
  }

  it should "solve part 2 test" in {
    Seats(testData).solve2() shouldBe 26
  }

  it should "solve part 2" in {
    Seats(Loader(this, "day11.txt")).solve2() shouldBe 2208
  }
}
