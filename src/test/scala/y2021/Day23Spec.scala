package y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day23Spec extends AnyFlatSpec with Matchers {
  private lazy val testInput = util.Loader(this, "day23.test.txt").toSeq
  private lazy val input = util.Loader(this, "day23.txt").toSeq

  it should "pass part 1 tests" in {
    new Day23Part1 {
      Config.part1(testInput).dijkstra(print = false) shouldBe 12521
    }
  }

  it should "pass part 1" in {
    new Day23Part1 {
      Config.part1(input).dijkstra(print = false) shouldBe 15516
    }
  }

  it should "pass part 2 tests" in {
    new Day23Part2 {
      Config.part2(testInput).dijkstra(print = false) shouldBe 44169
    }
  }

  it should "pass part 2" in {
    new Day23Part2 {
      Config.part2(input).dijkstra(print = true) shouldBe 45272
    }
  }
}
