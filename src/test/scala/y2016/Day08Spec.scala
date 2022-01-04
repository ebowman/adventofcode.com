package y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day08Spec extends AnyFlatSpec with Matchers with Day08 {
  private lazy val input = util.Loader(this, "day08.txt").toSeq
  it should "pass part 1" in {
    val display = solve(input)
    (display.lit, display.toString) shouldBe(121,
      "X|X|X| | |X| | |X| |X|X|X| | |X| | |X| | |X|X| | |X|X|X|X| | |X|X| | |X|X|X|X| | |X|X|X| |X| | | | \n" +
        "X| | |X| |X| | |X| |X| | |X| |X| | |X| |X| | |X| |X| | | | |X| | |X| |X| | | | | | |X| | |X| | | | \n" +
        "X| | |X| |X| | |X| |X| | |X| |X| | |X| |X| | | | |X|X|X| | |X| | |X| |X|X|X| | | | |X| | |X| | | | \n" +
        "X|X|X| | |X| | |X| |X|X|X| | |X| | |X| |X| | | | |X| | | | |X| | |X| |X| | | | | | |X| | |X| | | | \n" +
        "X| |X| | |X| | |X| |X| |X| | |X| | |X| |X| | |X| |X| | | | |X| | |X| |X| | | | | | |X| | |X| | | | \n" +
        "X| | |X| | |X|X| | |X| | |X| | |X|X| | | |X|X| | |X|X|X|X| | |X|X| | |X|X|X|X| | |X|X|X| |X|X|X|X| ")
  }
}
