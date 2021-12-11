package y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day06Spec extends AnyFlatSpec with Matchers with Day06 {
  lazy val testFish = LanternFish(Loader(this, "day06.test.txt").head)
  lazy val fish = LanternFish(Loader(this, "day06.txt").toSeq.head)

  "Day06" should "pass the part 1 tests" in {
    solve1(testFish, 80).size shouldBe 5934
  }

  it should "pass part 1" in {
    solve1(fish, 80).size shouldBe 373378
  }

  it should "pass part 2 test" in {
    solve2(testFish, 256) shouldBe 26984457539L
  }

  it should "pass part 2" in {
    solve2(fish, 256) shouldBe 1682576647495L
  }
}
