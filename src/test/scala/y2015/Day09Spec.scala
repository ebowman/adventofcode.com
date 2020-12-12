package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day09Spec extends AnyFlatSpec with Matchers with Day09 {

  "First test" should "pass" in {
    Loader(this, "day09.txt").foreach { line =>
      parseAll(edge, line).get
    }

    val allPaths = places.toSeq.permutations.map { path: Seq[String] =>
      (path, (for (edge <- path.sliding(2)) yield {
        paths(edge.head)(edge.tail.head)
      }).sum)
    }
    allPaths.minBy(_._2)._2 shouldBe 117
  }

  "Second test" should "pass" in {
    Loader(this, "day09.txt").foreach { line =>
      parseAll(edge, line).get
    }

    val allPaths = places.toSeq.permutations.map { path: Seq[String] =>
      (path, (for (edge <- path.sliding(2)) yield {
        paths(edge.head)(edge.tail.head)
      }).sum)
    }
    allPaths.maxBy(_._2)._2 shouldBe 909
  }
}
