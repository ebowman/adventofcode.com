package y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day06Spec extends AnyFlatSpec with Matchers with Day06 {

  "Simple bit parsing" should "work" in {
    parseWord("a") shouldBe 1
    parseWord("ab") shouldBe 3
    parseWord("abcx") shouldBe 8388615
    parseWord("z") shouldBe 33554432
  }

  it should "count bits" in {
    countBits(1) shouldBe 1
    countBits(2) shouldBe 1
    countBits(3) shouldBe 2
    countBits(255) shouldBe 8
    countBits(256) shouldBe 1
  }

  lazy val testInput: Seq[Seq[String]] = lines(
    """abc
      |
      |a
      |b
      |c
      |
      |ab
      |ac
      |
      |a
      |a
      |a
      |a
      |
      |b""".stripMargin.trim.linesIterator.toIterable)

  lazy val input: Seq[Seq[String]] = lines(Loader(this, "day06.txt"))

  it should "solve the OR test" in {
    solveOr(testInput) shouldBe 11
  }

  it should "solve the AND test" in {
    solveAnd(testInput) shouldBe 6
  }

  it should "solve" in {
    solveOr(input) shouldBe 6259
  }

  it should "solve 2" in {
    solveAnd(input) shouldBe 3178
  }
}
