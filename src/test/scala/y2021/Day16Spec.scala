package y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader
import scodec.bits._

class Day16Spec extends AnyFlatSpec with Matchers with Day16 {
  lazy val input = Loader(this, "day16.txt").head

  "Day16" should "pass the part 1 tests" in {
    solve1("8A004A801A8002F478") shouldBe 16
    solve1("620080001611562C8802118E34") shouldBe 12
    solve1("C0015000016115A2E0802F182340") shouldBe 23
    solve1("A0016C880162017C3686B18A3D4780") shouldBe 31
  }

  it should "pass part 1" in {
    solve1(input) shouldBe 965
  }

  it should "pass part 2 test" in {
    solve2("C200B40A82") shouldBe 3
    solve2("04005AC33890") shouldBe 54
    solve2("880086C3E88112") shouldBe 7
    solve2("CE00C43D881120") shouldBe 9
    solve2("D8005AC2A8F0") shouldBe 1
    solve2("F600BC2D8F") shouldBe 0
    solve2("9C005AC2F8F0") shouldBe 0
    solve2("9C0141080250320F1802104A08") shouldBe 1
  }

  it should "pass part 2" in {
    solve2(input) shouldBe 116672213160L
  }
}
