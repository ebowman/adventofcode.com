package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day16Spec extends AnyFlatSpec with Matchers with Day16 {
  lazy val output = Map(
    "children" -> 3,
    "cats" -> 7,
    "samoyeds" -> 2,
    "pomeranians" -> 3,
    "akitas" -> 0,
    "vizslas" -> 0,
    "goldfish" -> 5,
    "trees" -> 3,
    "cars" -> 2,
    "perfumes" -> 1)

  "Some basics" should "work" in {
    // Sue 3: cars: 5, pomeranians: 4, vizslas: 1
    parseAll(sue, "Sue 3: cars: 5, pomeranians: 4, vizslas: 1").get shouldEqual Sue(
      3, Map("cars" -> 5, "pomeranians" -> 4, "vizslas" -> 1))
  }
  it should "pass the real test" in {
    val sues = Loader(this, "day16.txt").map { line =>
      parseAll(sue, line).get
    }
    sues.map(sue => (sue, sue.score(output))).maxBy(_._2)._1.num shouldBe 213
  }
  it should "pass the second test" in {
    val sues = Loader(this, "day16.txt").map { line =>
      parseAll(sue, line).get
    }
    sues.map(sue => (sue, sue.score2(output))).maxBy(_._2)._1.num shouldBe 323
  }

}
