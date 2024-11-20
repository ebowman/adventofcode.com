package y2017

import scala.annotation.tailrec


trait Day23:
  enum Instruction:
    case Set(x: String, y: String)
    case Sub(x: String, y: String)
    case Mul(x: String, y: String)
    case Jnz(x: String, y: String)

  case class ProgramState(registers: Map[String, Long] = Map().withDefaultValue(0L),
                          position: Int = 0,
                          mulCount: Int = 0)

  def parseInstruction(line: String): Instruction =
    line.split(" ").toList match
      case "set" :: x :: y :: Nil => Instruction.Set(x, y)
      case "sub" :: x :: y :: Nil => Instruction.Sub(x, y)
      case "mul" :: x :: y :: Nil => Instruction.Mul(x, y)
      case "jnz" :: x :: y :: Nil => Instruction.Jnz(x, y)
      case _ => throw IllegalArgumentException(s"Invalid instruction: $line")

  def getValue(s: String, registers: Map[String, Long]): Long =
    s.toLongOption.getOrElse(registers(s))

  def executeInstruction(state: ProgramState, instruction: Instruction): ProgramState =
    import Instruction.*
    instruction match
      case Set(x, y) =>
        state.copy(
          registers = state.registers + (x -> getValue(y, state.registers)),
          position = state.position + 1
        )
      case Sub(x, y) =>
        state.copy(
          registers = state.registers + (x -> (state.registers(x) - getValue(y, state.registers))),
          position = state.position + 1
        )
      case Mul(x, y) =>
        state.copy(
          registers = state.registers + (x -> (state.registers(x) * getValue(y, state.registers))),
          position = state.position + 1,
          mulCount = state.mulCount + 1
        )
      case Jnz(x, y) =>
        if getValue(x, state.registers) != 0 then
          state.copy(position = state.position + getValue(y, state.registers).toInt)
        else
          state.copy(position = state.position + 1)

  def runProgram(instructions: Seq[String], initialA: Int = 0): ProgramState =
    val program = instructions.map(parseInstruction)

    @tailrec
    def loop(state: ProgramState): ProgramState =
      if state.position >= program.length then state
      else loop(executeInstruction(state, program(state.position)))

    val initialState = ProgramState(Map("a" -> initialA.toLong).withDefaultValue(0L))
    loop(initialState)

  def solvePart1(input: Seq[String]): Int =
    runProgram(input).mulCount

  def isComposite(n: Long): Boolean =
    n > 1 && (2L to math.sqrt(n).toLong).exists(n % _ == 0)

  def solvePart2(input: Seq[String]): Int =
    val initialState = runProgram(input.take(8), 1)
    val b = initialState.registers("b")
    val c = initialState.registers("c")
    val step = 17L

    LazyList.iterate(b)(_ + step)
      .takeWhile(_ <= c)
      .count(isComposite)

end Day23