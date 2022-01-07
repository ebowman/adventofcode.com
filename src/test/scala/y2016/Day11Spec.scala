package y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day11Spec extends AnyFlatSpec with Matchers with Day11 {
  private lazy val input = util.Loader(this, "day11.txt").toSeq
  private lazy val testInput = util.Loader(this, "day11.test.txt").toSeq

  it should "solve part 1 tests" in {
    solve(testInput) shouldBe 11
  }
  it should "solve part 1" in {
    solve(input) shouldBe 31
  }
  it should "solve part 2" in {
    solve(input, Seq("El", "Di").flatMap(name => Seq(Generator(name), Microchip(name)))) shouldBe 55
  }
}
