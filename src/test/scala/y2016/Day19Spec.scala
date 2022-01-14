package y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day19Spec extends AnyFlatSpec with Matchers with Day19 {
  it should "solve part 1" in {
    solve1(3014387) shouldBe 1834471
  }
  it should "solve part 2" in {
    solve2(3014387) shouldBe 1420064
  }
}
