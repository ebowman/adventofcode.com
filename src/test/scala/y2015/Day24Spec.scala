package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day24Spec extends AnyFlatSpec with Matchers with Day24 {

  lazy val testInput = Loader(this, "day24.test.txt").toSeq
  lazy val input  = Loader(this, "day24.txt").toSeq

  it should "solve part 1 test" in {
    solve(testInput, 3) shouldBe 99
  }

  it should "solve part 1" in {
    solve(input, 3) shouldBe 11846773891L
  }

  it should "solve part 2 test" in {
    solve(testInput, 4) shouldBe 44
  }
  it should "solve part 2" in {
    println(solve(input, 4))
  }
}

