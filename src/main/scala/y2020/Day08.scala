package y2020

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers


trait Day08 extends RegexParsers {

  object Compiler extends RegexParsers {

    sealed trait Instruction

    case class Nop(x: Int) extends Instruction

    case class Jmp(x: Int) extends Instruction

    case class Acc(x: Int) extends Instruction

    def nop: Parser[Nop] = "nop" ~> "[+-]?\\d+".r ^^ { x => Nop(x.toInt) }

    def jmp: Parser[Jmp] = "jmp" ~> "[+-]?\\d+".r ^^ { x => Jmp(x.toInt) }

    def acc: Parser[Acc] = "acc" ~> "[+-]?\\d+".r ^^ { x => Acc(x.toInt) }

    def allTokens: Parser[Instruction] = nop | jmp | acc

    def compile(program: Iterable[String]): IndexedSeq[Instruction] =
      program.map { line => parseAll(allTokens, line).get }.toIndexedSeq
  }

  import Compiler.{Instruction, Nop, Jmp, Acc}

  case class MetaMachine(instructions: IndexedSeq[Instruction]) {
    def nopsAndJumps(instructions: Seq[Instruction]): Seq[Int] =
      instructions.zipWithIndex.filter(x => x._1.isInstanceOf[Nop] || x._1.isInstanceOf[Jmp]).map(_._2)

    def run: Int = Machine(instructions).run match {
      case (true, accum) => accum
      case (false, _) => ChangeMachine(instructions, nopsAndJumps(instructions)).solve
    }

    private case class ChangeMachine(instructions: IndexedSeq[Instruction], nopsAndJumps: Seq[Int], result: Int = -1) {
      def hasNext: Boolean = result == -1

      def flip(instructions: IndexedSeq[Instruction], c: Int): IndexedSeq[Instruction] = {
        instructions(c) match {
          case Nop(x) => (instructions.take(c) :+ Jmp(x)) ++ instructions.drop(c + 1)
          case Jmp(x) => (instructions.take(c) :+ Nop(x)) ++ instructions.drop(c + 1)
        }
      }

      def next: ChangeMachine = {
        Machine(flip(instructions, nopsAndJumps.head)).run match {
          case (true, accum) => ChangeMachine(instructions, nopsAndJumps, accum)
          case (false, _) => ChangeMachine(instructions, nopsAndJumps.tail, result)
        }
      }

      @tailrec
      final def solve: Int = if (hasNext) this.next.solve else result
    }

  }

  case class Machine(instructions: IndexedSeq[Instruction]) {
    def run: (Boolean, Int) = {
      @tailrec
      def recurse(cursor: Int = 0, accum: Int = 0, seen: Set[Int] = Set()): (Boolean, Int) = {
        if (cursor >= instructions.length) (true, accum)
        else if (seen.contains(cursor)) (false, accum)
        else {
          instructions(cursor) match {
            case Nop(_) => recurse(cursor + 1, accum, seen + cursor)
            case Acc(n) => recurse(cursor + 1, accum + n, seen + cursor)
            case Jmp(n) => recurse(cursor + n, accum, seen + cursor)
          }
        }
      }

      recurse()
    }
  }

}
