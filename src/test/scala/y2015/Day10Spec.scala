package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day10Spec extends AnyFlatSpec with Matchers with Day10 {
  it should "solve the puzzle" in {
    solve("1113122113", 40).length shouldBe 360154
  }

  it should "solve part 2" in {
    solve("1113122113", 50).length shouldBe 5103798
  }
}
