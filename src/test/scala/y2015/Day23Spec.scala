package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day23Spec extends AnyFlatSpec with Matchers with Day23 {

  it should "solve part 1 test" in {
    Machine(Loader(this, "day23.test.txt").toIndexedSeq).terminate.regs._1 shouldBe 2
  }

  it should "solve part 1" in {
    Machine(Loader(this, "day23.txt").toIndexedSeq).terminate.regs._2 shouldBe 255
  }

  it should "solve part 2" in {
    Machine(Loader(this, "day23.txt").toIndexedSeq, regs = (1, 0)).terminate.regs._2 shouldBe 334
  }
}

