package y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day01Spec extends AnyFlatSpec with Matchers {

  "Sum2020" should "confirm the example in the docs" in {
    val inputs = Set(1721, 979, 366, 299, 675, 1456)
    Day01.s2(inputs) shouldBe 514579
    Day01.s3(inputs) shouldBe 241861950
  }

  it should "get the right answer for my personal inputs" in {
    Day01.sum2 shouldBe 987339
    Day01.sum3 shouldBe 259521570
  }
}
