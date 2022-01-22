package y2019

import scala.annotation.tailrec

trait Day02 {

  object Intcode {
    def compile(input: String, reps: Seq[(Int, Int)] = Seq.empty): Intcode = {
      val mem = input.split(",").map(_.toInt)
      reps.foreach { case (addr, value) => mem(addr) = value }
      Intcode(mem)
    }
  }

  case class Intcode(memory: Array[Int]) {
    @tailrec final def execute(cursor: Int = 0): Intcode = {
      memory.slice(cursor, cursor + 4) match {
        case Array(1, r1, r2, st) =>
          memory(st) = memory(r1) + memory(r2)
          execute(cursor + 4)
        case Array(2, r1, r2, st) =>
          memory(st) = memory(r1) * memory(r2)
          execute(cursor + 4)
        case stop if stop.head == 99 => this
      }
    }
  }

  def part1(input: String): Int = Intcode.compile(input, Seq(1 -> 12, 2 -> 2)).execute().memory(0)

  def part2(input: String): Int = {
    (for
      noun <- 0 to 99
      verb <- 0 to 99
      output = Intcode.compile(input, Seq(1 -> noun, 2 -> verb)).execute().memory(0) if output == 19690720
    yield 100 * noun + verb).head
  }
}
