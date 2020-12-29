package y2019

import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}
import scala.annotation.tailrec
import scala.util.Try

trait Day09 {

  object Intcode {

    private def compileStr(code: String, bytes: Int): Array[Long] = {
      val tmp = code.split(",").map(_.toLong)
      tmp ++ new Array[Long](bytes - tmp.length)
    }

    def compiler(code: String, bytes: Int = 2048): Compiler = Compiler(code, bytes)

    case class Compiler(code: String, bytes: Int) {
      def compile(inputs: Seq[Long] = Seq.empty): Intcode = {
        val compiled = compileStr(code, bytes)
        val input = new LinkedBlockingQueue[Long]
        inputs.foreach(input.put)
        Intcode(compiled, input, new LinkedBlockingQueue[Long])
      }
    }

  }

  case class Intcode(memory: Array[Long], inputs: BlockingQueue[Long], outputs: BlockingQueue[Long]) {

    var relativeBase: Int = 0

    case class PackedInstruction(byte: Long) {
      lazy val str: String = byte.toString.reverse
      lazy val opcode: Int = str.take(2).reverse.toInt
      lazy val mode1: Int = Try(str.slice(2, 3).charAt(0) - '0').getOrElse(0)
      lazy val mode2: Int = Try(str.slice(3, 4).charAt(0) - '0').getOrElse(0)
      lazy val mode3: Int = Try(str.slice(4, 5).charAt(0) - '0').getOrElse(0)
    }

    @tailrec final def execute(cursor: Int = 0): Unit = {

      def read(c: Int, mode: Int): Long = {
        val next = memory(c)
        mode match {
          case 0 => memory(next.toInt)
          case 1 => next
          case 2 => memory((relativeBase + next).toInt)
        }
      }

      def write(value: Long, c: Int, mode: Int): Unit = {
        val next = memory(c)
        mode match {
          case 0 => memory(memory(c).toInt) = value
          case 2 => memory(relativeBase + memory(c).toInt) = value
        }
      }

      val (iADD, iMUL, iIN, iOUT, iJIT, iJIF, iSTLT, iSTEQ, iARB) = (1, 2, 3, 4, 5, 6, 7, 8, 9)
      PackedInstruction(memory(cursor)) match {
        case op if op.opcode == iADD =>
          val op1 = read(cursor + 1, op.mode1)
          val op2 = read(cursor + 2, op.mode2)
          write(op1 + op2, cursor+ 3, op.mode3)
          execute(cursor + 4)
        case op if op.opcode == iMUL =>
          val op1 = read(cursor + 1, op.mode1)
          val op2 = read(cursor + 2, op.mode2)
          write(op1 * op2, cursor+ 3, op.mode3)
          execute(cursor + 4)
        case op if op.opcode == iIN =>
          val input = inputs.take()
          write(input, cursor+ 1, op.mode1)
          execute(cursor + 2)
        case op if op.opcode == iOUT =>
          val output = read(cursor + 1, op.mode1)
          outputs.put(output)
          execute(cursor + 2)
        case op if op.opcode == iJIT =>
          val op1 = read(cursor + 1, op.mode1)
          val op2 = read(cursor + 2, op.mode2)
          if (op1 != 0) execute(op2.toInt)
          else execute(cursor + 3)
        case op if op.opcode == iJIF =>
          val op1 = read(cursor + 1, op.mode1)
          val op2 = read(cursor + 2, op.mode2)
          if (op1 == 0) execute(op2.toInt)
          else execute(cursor + 3)
        case op if op.opcode == iSTLT =>
          val op1 = read(cursor + 1, op.mode1)
          val op2 = read(cursor + 2, op.mode2)
          val value = if (op1 < op2) 1 else 0
          write(value, cursor + 3, op.mode3)
          execute(cursor + 4)
        case op if op.opcode == iSTEQ =>
          val op1 = read(cursor + 1, op.mode1)
          val op2 = read(cursor + 2, op.mode2)
          val value = if (op1 == op2) 1 else 0
          write(value, cursor + 3, op.mode3)
          execute(cursor + 4)
        case op if op.opcode == iARB => // adjust relative base
          val op1 = read(cursor + 1, op.mode1)
          relativeBase += op1.toInt
          execute(cursor + 2)
        case stop if stop.opcode == 99 =>
          ()
      }
    }
  }

}
