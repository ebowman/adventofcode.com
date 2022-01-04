package y2016

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class Day05Spec extends AnyFlatSpec with Matchers with Day05 {

  it should "pass part 1" in {
    solve("ugkcyxxp") shouldBe ("d4cd2ee1", "f2c730e5")
  }
}
