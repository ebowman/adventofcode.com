package y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day14Spec extends AnyFlatSpec with Matchers with Day14 {

  lazy val input: IndexedSeq[String] = Loader(this, "day14.txt").toIndexedSeq

  it should "solve part 1 test" in {
    part1(
      """
        |mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
        |mem[8] = 11
        |mem[7] = 101
        |mem[8] = 0""".stripMargin.trim.linesIterator.toIndexedSeq) shouldBe 165L
  }

  it should "solve part 1" in {
    part1(Loader(this, "day14.txt").toIndexedSeq) shouldBe 15514035145260L
  }

  it should "solve part 2 test" in {
    part2(
      """
        |mask = 000000000000000000000000000000X1001X
        |mem[42] = 100
        |mask = 00000000000000000000000000000000X0XX
        |mem[26] = 1""".stripMargin.trim.linesIterator.toIndexedSeq) shouldBe 208L
  }

  it should "solve part 2" in {
    part2(Loader(this, "day14.txt").toIndexedSeq) shouldBe 3926790061594L
  }
}
