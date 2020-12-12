package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day17Spec extends AnyFlatSpec with Matchers with Day17 {

  "Simple case" should "work" in {
    solve(Seq(20, 15, 10, 5, 5), 25) shouldBe 4
  }
  it should "solve the second simple case" in {
    solveMin(Seq(20, 15, 10, 5, 5), 25) shouldBe 3
  }
  it should "solve the real case" in {
    solve(Loader(this,"day17.txt").map(_.toInt).toSeq.sorted, 150) shouldBe 654
  }
  it should "solve the second case" in {
    solveMin(Loader(this,"day17.txt").map(_.toInt).toSeq.sorted, 150) shouldBe 57
  }
}
