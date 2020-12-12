package y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day05Spec extends AnyFlatSpec with Matchers with Day05 {
  "SeatFinder" should "solve known test cases" in {
    SeatFinder("FBFBBFFRLR").seat shouldBe(44, 5)
    SeatFinder("FBFBBFFRLR").id shouldBe 357

    SeatFinder("BFFFBBFRRR").seat shouldBe(70, 7)
    SeatFinder("BFFFBBFRRR").id shouldBe 567

    SeatFinder("FFFBBBFRRR").seat shouldBe(14, 7)
    SeatFinder("FFFBBBFRRR").id shouldBe 119

    SeatFinder("BBFFBBFRLL").seat shouldBe(102, 4)
    SeatFinder("BBFFBBFRLL").id shouldBe 820
  }

  it should "solve the first puzzle" in {
    Loader(this, "day05.txt").map(id => SeatFinder(id)).maxBy(_.id).id shouldBe 919
  }
  it should "solve the second part" in {
    val allTickets = Loader(this, "day05.txt").map(id => SeatFinder(id)).map(sf => (sf.id, sf.code)).toMap
    Range(allTickets.keys.min, allTickets.keys.max + 1).find(id => !allTickets.contains(id)) shouldBe Some(642)
  }
}
