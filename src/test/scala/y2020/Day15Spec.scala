package y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day15Spec extends AnyFlatSpec with Matchers with Day15 {

  lazy val input: IndexedSeq[String] = Loader(this, "day15.txt").toIndexedSeq

  it should "solve part 1 test" in {
    solve("0,3,6",2020 ) shouldBe 436
    solve("1,3,2", 2020 ) shouldBe 1
    solve("2,1,3", 2020 ) shouldBe 10
    solve("1,2,3", 2020) shouldBe 27
    solve("2,3,1", 2020) shouldBe 78
    solve("3,2,1", 2020) shouldBe 438
    solve("3,1,2", 2020) shouldBe 1836
  }

  it should "solve part 1" in {
    solve(Loader(this, "day15.txt").mkString, 2020) shouldBe 1522
  }

  it should "solve part 2 test" in {
    solve("0,3,6",30000000 ) shouldBe 175594
    solve("1,3,2", 30000000 ) shouldBe 2578
    solve("2,1,3", 30000000 ) shouldBe 3544142
    solve("1,2,3", 30000000) shouldBe 261214
    solve("2,3,1", 30000000) shouldBe 6895259
    solve("3,2,1", 30000000) shouldBe 18
    solve("3,1,2", 30000000) shouldBe 362
  }

  it should "solve part 2" in {
    solve(Loader(this, "day15.txt").mkString, 30000000) shouldBe 18234
  }
}
