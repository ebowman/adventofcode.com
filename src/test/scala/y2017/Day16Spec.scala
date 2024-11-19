package y2017

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day16Spec extends AnyFlatSpec with Matchers with Day16:

  lazy val input: IndexedSeq[String] = Loader(this, "day16.txt").toIndexedSeq
  lazy val testInput: IndexedSeq[String] = Loader(this, "day16.test.txt").toIndexedSeq

  it should "solve part 1 test" in:
    solvePart1(testInput, 5) shouldBe "baedc"
    
  it should "solve part 1" in:
    solvePart1(input) shouldBe "lgpkniodmjacfbeh"

  it should "solve part 2" in:
    solvePart2(input) shouldBe "hklecbpnjigoafmd"
