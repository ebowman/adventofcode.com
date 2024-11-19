package y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*
import util.Loader

class Day10Spec extends AnyFlatSpec with Matchers with Day10:

  lazy val input: IndexedSeq[String] = Loader(this, "day10.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day10.test.txt").toIndexedSeq

  it should "solve part 1 test" in :
    solvePart1(testInput) shouldBe 12

  it should "solve part 1" in :
    solvePart1(input) shouldBe 4480

  it should "solve part 2 test cases" in :
    solvePart2(Seq("")) shouldBe "a2582a3a0e66e6e86e3812dcb672a272"
    solvePart2(Seq("AoC 2017")) shouldBe "33efeb34ea91902bb2f59c9920caa6cd"
    solvePart2(Seq("1,2,3")) shouldBe "3efbe78a8d82f29979031a4aa0b16a9d"
    solvePart2(Seq("1,2,4")) shouldBe "63960835bcdc130f0b66d7ff4f6a5a8e"

  it should "solve part 2" in :
    solvePart2(input) shouldBe "c500ffe015c83b60fad2e4b7d59dabc4"
