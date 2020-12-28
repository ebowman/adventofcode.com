package y2019

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day07Spec extends AnyFlatSpec with Matchers with Day07 {
  lazy val code: String = Loader.is(this, "day07.txt").head

  it should "solve part 1 tests" in {
    executeChain("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0", Seq(4, 3, 2, 1, 0)) shouldBe 43210
    executeChain("3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0",
      Seq(0, 1, 2, 3, 4)) shouldBe 54321
    executeChain("3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31," +
      "1,32,31,31,4,31,99,0,0,0", Seq(1, 0, 4, 3, 2)) shouldBe 65210
  }

  it should "solve part 1" in {
    (0 to 4).permutations.map(phases => executeChain(code, phases)).max shouldBe 212460
  }

  it should "solve part 2 test" in {
    executeFeedback("3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5",
      Seq(9, 8, 7, 6, 5)) shouldBe 139629729
  }

  it should "solve part 2" in {
    (5 to 9).permutations.map(phases => executeFeedback(code, phases)).max shouldBe 21844737
  }
}
