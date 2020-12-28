package y2019

import scala.annotation.tailrec

trait Day05 {

  object Intcode {
    def compile(input: String): Intcode = {
      val mem = input.split(",").map(_.toInt)
      Intcode(mem)
    }
  }

  case class PackedInstruction(byte: Int) {
    lazy val str: String = byte.toString.reverse
    lazy val opcode: Int = str.take(2).reverse.toInt
    lazy val mode1: Boolean = str.slice(2, 3) == "1"
    lazy val mode2: Boolean = str.slice(3, 4) == "1"
    lazy val mode3: Boolean = str.slice(4, 5) == "1"
  }

  case class Intcode(memory: Array[Int]) {
    @tailrec final def execute(cursor: Int = 0, input: Int = 0, output: Int = 0): Int = {
      def read(c: Int, mode: Boolean): Int = if (mode) memory(c) else memory(memory(c))

      val (iADD, iMUL, iIN, iOUT, iJIT, iJIF, iSTLT, iSTEQ) = (1, 2, 3, 4, 5, 6, 7, 8)
      PackedInstruction(memory(cursor)) match {
        case op if op.opcode == iADD =>
          val op1 = read(cursor + 1, op.mode1)
          val op2 = read(cursor + 2, op.mode2)
          memory(memory(cursor + 3)) = op1 + op2
          execute(cursor + 4, input, output)
        case op if op.opcode == iMUL =>
          val op1 = read(cursor + 1, op.mode1)
          val op2 = read(cursor + 2, op.mode2)
          memory(memory(cursor + 3)) = op1 * op2
          execute(cursor + 4, input, output)
        case op if op.opcode == iIN =>
          memory(memory(cursor + 1)) = input
          execute(cursor + 2, input, output)
        case op if op.opcode == iOUT =>
          execute(cursor + 2, input, memory(memory(cursor + 1)))
        case op if op.opcode == iJIT =>
          val op1 = read(cursor + 1, op.mode1)
          val op2 = read(cursor + 2, op.mode2)
          if (op1 != 0) execute(op2, input, output)
          else execute(cursor + 3, input, output)
        case op if op.opcode == iJIF =>
          val op1 = read(cursor + 1, op.mode1)
          val op2 = read(cursor + 2, op.mode2)
          if (op1 == 0) execute(op2, input, output)
          else execute(cursor + 3, input, output)
        case op if op.opcode == iSTLT =>
          val op1 = read(cursor + 1, op.mode1)
          val op2 = read(cursor + 2, op.mode2)
          if (op1 < op2) memory(memory(cursor + 3)) = 1
          else memory(memory(cursor + 3)) = 0
          execute(cursor + 4, input, output)
        case op if op.opcode == iSTEQ =>
          val op1 = read(cursor + 1, op.mode1)
          val op2 = read(cursor + 2, op.mode2)
          if (op1 == op2) memory(memory(cursor + 3)) = 1
          else memory(memory(cursor + 3)) = 0
          execute(cursor + 4, input, output)
        case stop if stop.opcode == 99 => output
      }
    }
  }

  def part1(code: String): Int = Intcode.compile(code).execute(input = 1)

  def part2(code: String): Int = Intcode.compile(code).execute(input = 5)
}
