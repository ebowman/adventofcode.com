package y2016

import scala.annotation.tailrec
import scala.util.matching.Regex

trait Day12 {
  val Cpy: Regex = """cpy (-?\w+) (\w)""".r
  val Inc: Regex = """inc (\w)""".r
  val Dec: Regex = """dec (\w)""".r
  val Jnz: Regex = """jnz (-?\w+) (-?\d+)""".r

  case class Machine(program: Seq[String], cursor: Int = 0, regs: Map[String, Int]) {
    def next: Either[Machine, Map[String, Int]] = {
      if cursor >= program.size then Right(regs)
      else {
        Left(program(cursor) match {
          case Cpy(r1, r2) =>
            copy(cursor = cursor + 1, regs = regs + (r2 -> (if r1.head.isLetter then regs(r1) else r1.toInt)))
          case Inc(reg) => copy(cursor = cursor + 1, regs = regs + (reg -> (regs(reg) + 1)))
          case Dec(reg) => copy(cursor = cursor + 1, regs = regs + (reg -> (regs(reg) - 1)))
          case Jnz(reg, jmp) => copy(cursor = cursor +
            (if reg.head.isLetter then if regs(reg) == 0 then 1 else jmp.toInt else if reg.toInt == 0 then 1 else jmp.toInt))
          case "" => copy(cursor = cursor + 1)
        })
      }
    }

    @tailrec final def execute: Map[String, Int] = {
      next match {
        case Left(machine) => machine.execute
        case Right(regs) => regs
      }
    }
  }

  def solve(input: Seq[String], r: Map[String, Int] = Map().withDefaultValue(0)): Int =
    Machine(input, regs = r).execute("a")
}
