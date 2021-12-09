package y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day06Spec extends AnyFlatSpec with Matchers with Day06 {
  lazy val testInput = Loader(this, "day06.test.txt").toSeq
  lazy val input = Loader(this, "day06.txt").toSeq

  "Day06" should "pass the part 1 tests" in {
    val fish = load(testInput.head)
    recurse(fish, 80).size shouldBe 5934
  }

  it should "pass part 1" in {
    val fish = load(input.head)
    recurse(fish, 80).size shouldBe 373378
  }

  it should "pass part 2 test" in {
    val fish = load(testInput.head)
    solve2(fish, 256) shouldBe 26984457539L
  }

  it should "pass part 2" in {
    val fish = load(input.head)
    solve2(fish, 256) shouldBe 1682576647495L
  }
}
