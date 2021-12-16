package y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day08Spec extends AnyFlatSpec with Matchers with Day08 {
  lazy val testInput: IndexedSeq[Compiler.Instruction] = Compiler.compile(
    """
      |nop +0
      |acc +1
      |jmp +4
      |acc +3
      |jmp -3
      |acc -99
      |acc +1
      |jmp -4
      |acc +6
      |""".stripMargin.trim.linesIterator.iterator.to(Iterable))

  lazy val input: IndexedSeq[Compiler.Instruction] = Compiler.compile(Loader(this, "day08.txt"))

  it should "pass the test case" in {
    Machine(testInput).run shouldBe(false, 5)
  }

  it should "solve the puzzle" in {
    Machine(input).run shouldBe(false, 1262)
  }

  it should "solve the second test case" in {
    MetaMachine(testInput).run shouldBe 8

  }
  it should "solve the second puzzle" in {
    MetaMachine(input).run shouldBe 1643
  }
}
