package y2019

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day02Spec extends AnyFlatSpec with Matchers with Day02 {

  lazy val input: IndexedSeq[String] = Loader.is(this, "day02.txt")

  it should "solve part 1 test" in {
    Intcode.compile("1,9,10,3,2,3,11,0,99,30,40,50").execute().memory.toSeq shouldBe
      Seq(3500,9,10,70, 2,3,11,0, 99, 30,40,50)
    Intcode.compile("1,0,0,0,99").execute().memory.toSeq shouldBe Seq(2,0,0,0,99)
    Intcode.compile("2,3,0,3,99").execute().memory.toSeq shouldBe Seq(2,3,0,6,99)
    Intcode.compile("2,4,4,5,99,0").execute().memory.toSeq shouldBe Seq(2,4,4,5,99,9801)
    Intcode.compile("1,1,1,4,99,5,6,0,99").execute().memory.toSeq shouldBe Seq(30,1,1,4,2,5,6,0,99)
  }

  it should "solve part 1" in {
    part1(input.head) shouldBe 5534943
  }

  it should "solve part 2" in {
    part2(input.head) shouldBe 7603
  }
}
