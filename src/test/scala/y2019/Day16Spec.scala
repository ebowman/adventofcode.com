package y2019

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day16Spec extends AnyFlatSpec with Matchers with Day16 {

  lazy val testInput = "12345678"
  lazy val input: String = Loader.is(this, "day16.txt").head

  it should "wave generate according to spec" in {
    val wg1 = WaveGenerator(1)
    wg1.next() shouldBe 1
    wg1.next() shouldBe 0
    wg1.next() shouldBe -1
    wg1.next() shouldBe 0
    wg1.next() shouldBe 1
    val wg2 = WaveGenerator(2)
    wg2.next() shouldBe 0
    wg2.next() shouldBe 1
    wg2.next() shouldBe 1
    wg2.next() shouldBe 0
    wg2.next() shouldBe 0
    wg2.next() shouldBe -1
    wg2.next() shouldBe -1
    wg2.next() shouldBe 0
    wg2.next() shouldBe 0
    wg2.next() shouldBe 1
    wg2.next() shouldBe 1
    val wg3 = WaveGenerator(3)
    wg3.next() shouldBe 0
    wg3.next() shouldBe 0
    wg3.next() shouldBe 1
    wg3.next() shouldBe 1
    wg3.next() shouldBe 1
    wg3.next() shouldBe 0
    wg3.next() shouldBe 0
    wg3.next() shouldBe 0
    wg3.next() shouldBe -1
    wg3.next() shouldBe -1
    wg3.next() shouldBe -1
    wg3.next() shouldBe 0
    wg3.next() shouldBe 0
    wg3.next() shouldBe 0
    wg3.next() shouldBe 1
    wg3.next() shouldBe 1
    wg3.next() shouldBe 1
  }

  it should "solve part 1" in {
    part1("12345678", 1) shouldBe "48226158"
    part1("12345678", 2) shouldBe "34040438"
    part1("12345678", 3) shouldBe "03415518"
    part1("12345678", 4) shouldBe "01029498"
    part1("80871224585914546619083218645595", 100) shouldBe "24176176"
    part1("19617804207202209144916044189917", 100) shouldBe "73745418"
    part1("69317163492948606335995924319873", 100) shouldBe "52432133"
    part1(input, 100) shouldBe "89576828"
  }

  it should "solve part 2" in {
    part2(input, 100) shouldBe "23752579"
  }
}
