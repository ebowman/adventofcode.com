package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day08Spec extends AnyFlatSpec with Matchers with Day08 {

  "Basic test" should "pass" in {
    this.escaped("\"\"") shouldBe(2, 0)
    this.escaped("\"abc\"") shouldBe(5, 3)
    this.escaped("\"aaa\\\"aaa\"") shouldBe(10, 7)
    this.escaped("\"\\x27\"") shouldBe(6, 1)
  }
  it should "pass the first part of the test" in {
    val lines = Loader(this, "day08.txt")
    lines.map(escaped).map(ab => ab._1 - ab._2).sum shouldBe 1333
  }

  "Part 2" should "pass the basic tests" in {
    this.encode("\"\"") shouldBe (2, 6)
    this.encode("\"abc\"") shouldBe (5, 9)
    this.encode("\"aaa\\\"aaa\"") shouldBe (10, 16)
    this.encode("\"\\x27\"") shouldBe (6, 11)
  }

  it should "pass the final test" in {
    val lines = Loader(this, "day08.txt")
    lines.map(encode).map(ab => ab._2 - ab._1).sum shouldBe 2046
  }

}
