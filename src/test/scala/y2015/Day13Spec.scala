package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day13Spec extends AnyFlatSpec with Matchers with Day13 {
 "Logic" should "pass the test case" in {
   val input =
     """
       |Alice would gain 54 happiness units by sitting next to Bob.
       |Alice would lose 79 happiness units by sitting next to Carol.
       |Alice would lose 2 happiness units by sitting next to David.
       |Bob would gain 83 happiness units by sitting next to Alice.
       |Bob would lose 7 happiness units by sitting next to Carol.
       |Bob would lose 63 happiness units by sitting next to David.
       |Carol would lose 62 happiness units by sitting next to Alice.
       |Carol would gain 60 happiness units by sitting next to Bob.
       |Carol would gain 55 happiness units by sitting next to David.
       |David would gain 46 happiness units by sitting next to Alice.
       |David would lose 7 happiness units by sitting next to Bob.
       |David would gain 41 happiness units by sitting next to Carol.""".stripMargin.trim.linesIterator.toSeq

   buildWorld(input).netHappiness shouldBe 330
 }
  it should "pass the part 1 input data" in {
    buildWorld(Loader(this, "day13.txt")).netHappiness shouldBe 664
    buildWorld(Loader(this, "day13.txt")).netAmbivalentMe shouldBe 640
  }
}
