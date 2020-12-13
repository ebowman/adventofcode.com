package y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day13Spec extends AnyFlatSpec with Matchers with Day13 {

  it should "solve part 1 test" in {
    new Solver1(
      """|939
         |7,13,x,x,59,x,31,19""".stripMargin.linesIterator.toIterable).solve shouldBe 295
  }
  it should "solve part 1" in {
    new Solver1(Loader(this, "day13.txt")).solve shouldBe 370
  }

  it should "solve part 2 test" in {
    Solver2.solve("7,13,x,x,59,x,31,19") shouldBe BigInt(1068781)
    Solver2.solve("17,x,13,19") shouldBe BigInt(3417)
    Solver2.solve("67,7,59,61") shouldBe BigInt(754018)
    Solver2.solve("67,x,7,59,61") shouldBe BigInt(779210)
    Solver2.solve("67,7,x,59,61") shouldBe BigInt(1261476)
    Solver2.solve("1789,37,47,1889") shouldBe BigInt("1202161486")
  }

  it should "solve part 2" in {
    new Solver2(Loader(this, "day13.txt")).solve2 shouldBe BigInt("894954360381385")
  }
}
