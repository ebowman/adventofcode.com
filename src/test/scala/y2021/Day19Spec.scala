package y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day19Spec extends AnyFlatSpec with Matchers with Day19 {
  private lazy val testInputSimple = util.Loader(this, "day19.test.2.txt").toSeq
  private lazy val testInput = util.Loader(this, "day19.test.txt").toSeq
  private lazy val input = util.Loader(this, "day19.txt").toSeq

  lazy val scannerMap = Map(
    testInputSimple -> orientScanners(load(testInputSimple)),
    testInput -> orientScanners(load(testInput)),
    input -> orientScanners(load(input))
  )

  override def scanner(input: Seq[String]): Seq[Scanner] = scannerMap(input)

  it should "pass the super simple part 1 test" in {
    solve1(testInputSimple) shouldBe 38
  }

  it should "pass part 1 tests" in {
    solve1(testInput) shouldBe 79
  }

  it should "pass part 1" in {
    solve1(input) shouldBe 355
  }

  it should "pass part 2 tests" in {
    solve2(testInput) shouldBe 3621
  }

  it should "pass part 2" in {
    solve2(input) shouldBe 10842
  }
}
