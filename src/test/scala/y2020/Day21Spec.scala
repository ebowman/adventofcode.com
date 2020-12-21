package y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader


class Day21Spec extends AnyFlatSpec with Matchers with Day21 {

  lazy val testInput: IndexedSeq[String] = Loader(this, "day21.test.txt").toIndexedSeq
  lazy val input: IndexedSeq[String] = Loader(this, "day21.txt").toIndexedSeq

  it should "solve part 1 test" in {
    part1(testInput) shouldBe 5
  }

  it should "solve part 1" in {
    part1(input) shouldBe 2307
  }

  it should "solve part 2 test" in {
    part2(testInput) shouldBe "mxmxvkd,sqjhc,fvjkl"
  }

  it should "solve part 2" in {
    part2(input) shouldBe "cljf,frtfg,vvfjj,qmrps,hvnkk,qnvx,cpxmpc,qsjszn"
  }

}
