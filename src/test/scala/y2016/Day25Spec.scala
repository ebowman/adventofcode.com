package y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*

class Day25Spec extends AnyFlatSpec with Matchers with Day25 {
  lazy val input: Seq[String] = util.Loader(this, "day25.txt").toSeq

  it should "solve part 1" in {
    solve(input) shouldBe 182
  }
  
}
