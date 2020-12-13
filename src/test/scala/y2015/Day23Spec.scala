package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day23Spec extends AnyFlatSpec with Matchers with Day23 {

  it should "solve part 1 test" in {
  }

  it should "solve part 1" in {
    println(Machine(Loader(this, "day23.txt").toIndexedSeq).terminate.regs._1)
  }


}

