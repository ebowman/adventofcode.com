package y2019

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day12Spec extends AnyFlatSpec with Matchers with Day12 {

  lazy val input: IndexedSeq[String] = Loader.is(this, "day12.txt")

  it should "solve part 1" in {
    System.load(input).iterate(1000).e shouldBe 5517
  }

  it should "solve part 2" in {
    part2(input) shouldBe 303070460651184L
  }
}
