package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day10Spec extends AnyFlatSpec with Matchers with Day10 {
  "Simple cases" should "pass" in {
    speaknsay("1") shouldBe "11"
    speaknsay("11") shouldBe "21"
    speaknsay("21") shouldBe "1211"
    speaknsay("1211") shouldBe "111221"
    speaknsay("121") shouldBe "111211"
    speaknsay("1113122113") shouldBe "311311222113"

    iterate("1", 5) shouldBe "312211"
  }

  it should "solve the puzzle" in {
    iterate("1113122113", 40).length shouldBe 360154
  }

  it should "solve part 2" in {
    iterate("1113122113", 50).length shouldBe 5103798
  }
}
