package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day14Spec extends AnyFlatSpec with Matchers with Day14 {
  lazy val comet = Reindeer("Comet", 14, 10, 127)
  lazy val dancer = Reindeer("Dancer", 16, 11, 162)

  "Reindeer" should "pass the test case" in {
    // [move][rest][move][rest]
    // [10][127][1]
    // [10][127][10][16]
    // time: 0 10  137 147 274 275
    // dist: 0 140 140 280 280 294
    comet.dist(10) shouldBe 140
    comet.dist(137) shouldBe 140
    comet.dist(138) shouldBe 154
    comet.dist(147) shouldBe 280
    comet.dist(274) shouldBe 280
    comet.dist(275) shouldBe 294
    comet.dist(276) shouldBe 308

    comet.dist(1) shouldBe 14
    dancer.dist(1) shouldBe 16
    comet.dist(10) shouldBe 140
    dancer.dist(10) shouldBe 160
    comet.dist(11) shouldBe 140
    dancer.dist(11) shouldBe 176
    comet.dist(12) shouldBe 140
    dancer.dist(12) shouldBe 176
    comet.dist(138) shouldBe 154
    dancer.dist(174) shouldBe 192

    comet.dist(1000) shouldBe 1120
    dancer.dist(1000) shouldBe 1056

    comet.dist(2503) shouldBe 2660
    dancer.dist(2503) shouldBe 2640
  }
  "Part 2" should "be solved" in {
    val race = Race(Seq(comet, dancer))
    race.raceUntil(140) shouldBe 139
    race.raceUntil(1000) shouldBe 689
  }
  it should "pass the part 2 test" in {
    Race(Loader(this, "day14.txt").map(mkDeer).toSeq).raceUntil(2503) shouldBe 1256
  }
}
