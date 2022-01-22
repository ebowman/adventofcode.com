package y2015

import scala.annotation.tailrec

trait Day23 {
  val Inc = """inc ([a-b])""".r
  val Jmp = """jmp ([+-]\d+)""".r
  val Jio = """jio ([a-b]), ([+-]\d+)""".r
  val Jie = """jie ([a-b]), ([+-]\d+)""".r
  val Tpl = """tpl ([a-b])""".r
  val Hlf = """hlf ([a-b])""".r

  case class Machine(instructions: IndexedSeq[String], cursor: Int = 0, regs: (Int, Int) = (0, 0)) {
    def reg(r: String) = if r == "a" then regs._1 else regs._2

    def next: Machine = {
      instructions(cursor) match {
        case Jmp(d) => copy(cursor = cursor + d.toInt)
        case Jio(r, d) => if reg(r)  == 1 then copy(cursor = cursor + d.toInt) else copy(cursor = cursor + 1)
        case Jie(r, d) => if (reg(r) & 1) == 0 then copy(cursor = cursor + d.toInt) else copy(cursor = cursor + 1)
        case Inc(r) => copy(cursor = cursor + 1, regs = {
          if r == "a" then (regs._1 + 1, regs._2) else (regs._1, regs._2 + 1)
        })
        case Tpl(r) => copy(cursor = cursor + 1, regs = {
          if r == "a" then (regs._1 * 3, regs._2) else (regs._1, regs._2 * 3)
        })
        case Hlf(r) => copy(cursor = cursor + 1, regs = {
          if r == "a" then (regs._1 / 2, regs._2) else (regs._1, regs._2 / 2)
        })
      }
    }

    def terminate: Machine = {
      @tailrec
      def recurse(m: Machine): Machine = {
        if m.cursor >= m.instructions.length then m
        else recurse(m.next)
      }

      recurse(this)
    }
  }

}