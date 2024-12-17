package y2024

import scala.annotation.tailrec

// See: https://adventofcode.com/2024/day/17
class Day17 extends util.Day(17):

  private type Registers = Map[Char, Long]
  private val instructions: Vector[Instruction] = Vector(
    ShiftRegister('A'), // Adv
    Bxl, Bst, Jnz, Bxc, Out,
    ShiftRegister('B'), // Bdv
    ShiftRegister('C') // Cdv
  )

  def solvePart1(input: IndexedSeq[String]): Any =
    val (ra, program) = parseInput(input)
    val initialRegisters = Map('A' -> ra)
    run(program, initialRegisters).mkString(",")

  private def parseInput(input: IndexedSeq[String]): (Long, Vector[Int]) =
    val ra = input.head.split(": ")(1).toLong
    val program = input.last.split(": ")(1).split(",").map(_.toInt).toVector
    (ra, program)
  end parseInput

  private def run(program: Vector[Int], registers: Registers): List[Long] =
    @tailrec
    def recurse(ptr: Int, regs: Registers, outputAcc: List[Long]): List[Long] =
      if ptr >= program.length then
        outputAcc
      else
        val inst = program(ptr)
        val param = if ptr + 1 < program.length then program(ptr + 1) else 0

        val (nextPtr, maybeOut, updatedRegs) = instructions(inst).exec(param, regs)
        val newPtr = if nextPtr >= 0 then nextPtr else ptr + 2
        val newOutput = maybeOut.fold(outputAcc)(outputAcc :+ _)

        recurse(newPtr, updatedRegs, newOutput)
    end recurse

    recurse(0, registers, Nil)
  end run

  def solvePart2(input: IndexedSeq[String]): Any =
    val (_, program) = parseInput(input)
    findA(program, program.length - 1, 0L).get

  private def findA(program: Vector[Int], cursor: Int, acc: Long): Option[Long] =
    def testCandidate(candidate: Int): Option[Long] =
      val candidateA = acc * 8 + candidate
      val testRegisters = Map('A' -> candidateA)
      if run(program, testRegisters) == program.drop(cursor).map(_.toLong) then
        if cursor == 0 then Some(candidateA)
        else findA(program, cursor - 1, candidateA)
      else None
    end testCandidate

    (0 until 8)
      .iterator
      .flatMap(testCandidate)
      .nextOption()
  end findA

  sealed trait Instruction:
    def exec(i: Int, registers: Registers): (Int, Option[Long], Registers)

  private case class ShiftRegister(targetReg: Char) extends Instruction:
    def exec(i: Int, regs: Registers): (Int, Option[Long], Registers) =
      val shift = regs.lookup(i)
      val updated = regs + (targetReg -> (regs('A') >> shift))
      (-1, None, updated)

  private case object Bxl extends Instruction:
    def exec(i: Int, regs: Registers): (Int, Option[Long], Registers) =
      val updated = regs + ('B' -> (regs('B') ^ i))
      (-1, None, updated)

  private case object Bst extends Instruction:
    def exec(i: Int, regs: Registers): (Int, Option[Long], Registers) =
      val updated = regs + ('B' -> (regs.lookup(i) & 7))
      (-1, None, updated)

  private case object Jnz extends Instruction:
    def exec(i: Int, regs: Registers): (Int, Option[Long], Registers) =
      if regs('A') == 0 then (-1, None, regs) else (i, None, regs)

  private case object Bxc extends Instruction:
    def exec(i: Int, regs: Registers): (Int, Option[Long], Registers) =
      val updated = regs + ('B' -> (regs('B') ^ regs('C')))
      (-1, None, updated)

  private case object Out extends Instruction:
    def exec(i: Int, regs: Registers): (Int, Option[Long], Registers) =
      val output = regs.lookup(i) & 7
      (-1, Some(output), regs)

  private given registersOps: AnyRef with
    extension (regs: Registers)
      def lookup(i: Int): Long = i match
        case 4 => regs('A')
        case 5 => regs('B')
        case 6 => regs('C')
        case _ => i

end Day17
