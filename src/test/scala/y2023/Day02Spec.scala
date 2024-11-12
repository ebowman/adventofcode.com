
package y2023

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day02Spec extends AnyFlatSpec with Matchers with Day02 {

  lazy val input: IndexedSeq[String] = Loader(this, "day02.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day02.test.txt").toIndexedSeq

  it should "solve part 1 test" in {
    solve(testInput) shouldBe 8
  }

  it should "solve part 1" in {
    solve(input) shouldBe 2879
  }

  it should "solve part 2 test" in {
    solve2(testInput) shouldBe 2286
  }

  it should "solve part 2" in {
    solve2(input) shouldBe 65122
  }
}
