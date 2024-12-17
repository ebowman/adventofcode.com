package y2024

import scala.annotation.tailrec

// See: https://adventofcode.com/2024/day/17
class Day17 extends util.Day(17):

  private type Registers = Map[Char, Long]
  private type Instruction = (Int, Registers) => (Int, Option[Long], Registers)
  private val instructions: Vector[Instruction] = Vector(
    /* adv */ shiftRegister('A'),
    /* bxl */ (param, regs) =>
      val updated = regs + ('B' -> (regs('B') ^ param))
      (-1, None, updated),
    /* bst */ (param, regs) =>
      val updated = regs + ('B' -> (regs.lookup(param) & 7))
      (-1, None, updated),
    /* jnz */ (param, regs) =>
      if regs('A') == 0 then (-1, None, regs) else (param, None, regs),
    /* bxc */ (_, regs) =>
      val updated = regs + ('B' -> (regs('B') ^ regs('C')))
      (-1, None, updated),
    /* out */ (param, regs) =>
      val output = regs.lookup(param) & 7
      (-1, Some(output), regs),
    /* bdv */ shiftRegister('B'),
    /* cdv */ shiftRegister('C')
  )

  private def shiftRegister(reg: Char)(param: Int, regs: Registers) =
    val shift = regs.lookup(param)
    val updated = regs + (reg -> (regs('A') >> shift))
    (-1, None, updated)

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
        val instIndex = program(ptr)
        val param = if ptr + 1 < program.length then program(ptr + 1) else 0
        val (nextPtr, maybeOut, updatedRegs) = instructions(instIndex)(param, regs)
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

  private given registersOps: AnyRef with
    extension (regs: Registers)
      def lookup(i: Int): Long = i match
        case 4 => regs('A')
        case 5 => regs('B')
        case 6 => regs('C')
        case _ => i
    end extension
end Day17
