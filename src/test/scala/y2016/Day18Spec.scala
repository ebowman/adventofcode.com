package y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day18Spec extends AnyFlatSpec with Matchers with Day18 {
  lazy val testInput: String = util.Loader(this, "day18.txt").head
  it should "solve part 1" in {
    solve(testInput, 40) shouldBe 1978
  }
  it should "solve part 2" in {
    solve(testInput, 400000) shouldBe 20003246
  }
}
