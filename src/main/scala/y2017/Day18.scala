package y2017

import scala.annotation.tailrec

trait Day18:
  enum Instruction:
    case Snd(x: String)
    case Set(x: String, y: String)
    case Add(x: String, y: String)
    case Mul(x: String, y: String)
    case Mod(x: String, y: String)
    case Rcv(x: String)
    case Jgz(x: String, y: String)

  // Class for Part 1 state
  private case class SoundState(registers: Map[String, Long] = Map().withDefaultValue(0),
                                pc: Int = 0,
                                lastSound: Option[Long] = None,
                                firstRecovered: Option[Long] = None)

  // Program state for Part 2
  class Program(val id: Int, val instructionCount: Int):
    private var _registers: Map[String, Long] = Map("p" -> id.toLong).withDefaultValue(0)
    private var _pc: Int = 0
    private val _queue = new scala.collection.mutable.ArrayDeque[Long]()
    private var _sendCount: Int = 0
    private var _isWaiting: Boolean = false

    def registers: Map[String, Long] = _registers

    def pc: Int = _pc

    def sendCount: Int = _sendCount

    def isWaiting: Boolean = _isWaiting

    def queueIsEmpty: Boolean = _queue.isEmpty

    def isTerminated: Boolean = _pc < 0 || _pc >= instructionCount

    def getValue(x: String): Long =
      if x.matches("-?\\d+") then x.toLong
      else _registers(x)

    def enqueue(value: Long): Unit =
      _queue.append(value)
      if _isWaiting then _isWaiting = false

    def executeInstruction(instruction: Instruction, otherProg: Program): Boolean =
      if isTerminated then return false

      import Instruction.*
      instruction match
        case Snd(x) =>
          otherProg.enqueue(getValue(x))
          _sendCount += 1
          _pc += 1
          true

        case Set(x, y) =>
          _registers = _registers + (x -> getValue(y))
          _pc += 1
          true

        case Add(x, y) =>
          _registers = _registers + (x -> (_registers(x) + getValue(y)))
          _pc += 1
          true

        case Mul(x, y) =>
          _registers = _registers + (x -> (_registers(x) * getValue(y)))
          _pc += 1
          true

        case Mod(x, y) =>
          val divisor = getValue(y)
          if divisor != 0 then
            _registers = _registers + (x -> (_registers(x) % divisor))
          _pc += 1
          true

        case Rcv(x) =>
          if _queue.isEmpty then
            _isWaiting = true
            false
          else
            _registers = _registers + (x -> _queue.removeHead())
            _isWaiting = false
            _pc += 1
            true

        case Jgz(x, y) =>
          if getValue(x) > 0 then
            val offset = getValue(y).toInt
            _pc += offset
          else
            _pc += 1
          true

  private def parseValue(s: String): String = s.trim

  private def parseInstruction(line: String): Instruction =
    val parts = line.split(" ").map(_.trim)
    parts(0) match
      case "snd" => Instruction.Snd(parseValue(parts(1)))
      case "set" => Instruction.Set(parseValue(parts(1)), parseValue(parts(2)))
      case "add" => Instruction.Add(parseValue(parts(1)), parseValue(parts(2)))
      case "mul" => Instruction.Mul(parseValue(parts(1)), parseValue(parts(2)))
      case "mod" => Instruction.Mod(parseValue(parts(1)), parseValue(parts(2)))
      case "rcv" => Instruction.Rcv(parseValue(parts(1)))
      case "jgz" => Instruction.Jgz(parseValue(parts(1)), parseValue(parts(2)))
      case _ => throw IllegalArgumentException(s"Unknown instruction: ${parts(0)}")

  private def getValue(x: String, registers: Map[String, Long]): Long =
    if x.matches("-?\\d+") then x.toLong
    else registers(x)

  def solvePart1(input: Seq[String]): Long =
    val instructions = input.map(parseInstruction).toVector

    def executeInstruction(state: SoundState, instruction: Instruction): SoundState =
      import Instruction.*

      instruction match
        case Snd(x) =>
          state.copy(
            lastSound = Some(getValue(x, state.registers)),
            pc = state.pc + 1
          )

        case Set(x, y) =>
          state.copy(
            registers = state.registers + (x -> getValue(y, state.registers)),
            pc = state.pc + 1
          )

        case Add(x, y) =>
          state.copy(
            registers = state.registers + (x -> (state.registers(x) + getValue(y, state.registers))),
            pc = state.pc + 1
          )

        case Mul(x, y) =>
          state.copy(
            registers = state.registers + (x -> (state.registers(x) * getValue(y, state.registers))),
            pc = state.pc + 1
          )

        case Mod(x, y) =>
          val divisor = getValue(y, state.registers)
          if divisor != 0 then
            state.copy(
              registers = state.registers + (x -> (state.registers(x) % divisor)),
              pc = state.pc + 1
            )
          else state.copy(pc = state.pc + 1)

        case Rcv(x) =>
          if getValue(x, state.registers) != 0 && state.firstRecovered.isEmpty then
            state.copy(firstRecovered = state.lastSound, pc = state.pc + 1)
          else
            state.copy(pc = state.pc + 1)

        case Jgz(x, y) =>
          if getValue(x, state.registers) > 0 then
            state.copy(pc = state.pc + getValue(y, state.registers).toInt)
          else
            state.copy(pc = state.pc + 1)

    @tailrec
    def runUntilRecovered(state: SoundState): SoundState =
      if state.pc < 0 || state.pc >= instructions.length || state.firstRecovered.isDefined then
        state
      else
        runUntilRecovered(executeInstruction(state, instructions(state.pc)))

    runUntilRecovered(SoundState()).firstRecovered.getOrElse(0L)

  def solvePart2(input: Seq[String]): Int =
    val instructions = input.map(parseInstruction).toVector

    val prog0 = Program(0, instructions.length)
    val prog1 = Program(1, instructions.length)

    def isDeadlocked: Boolean =
      (prog0.isWaiting && prog1.isWaiting && prog0.queueIsEmpty && prog1.queueIsEmpty) ||
        (prog0.isTerminated && prog1.isTerminated) ||
        (prog0.isTerminated && prog1.isWaiting && prog1.queueIsEmpty) ||
        (prog1.isTerminated && prog0.isWaiting && prog0.queueIsEmpty)

    def runUntilBlocked: Boolean =
      var madeProgress = false
      if !prog0.isTerminated && !prog0.isWaiting then
        madeProgress |= prog0.executeInstruction(instructions(prog0.pc), prog1)
      if !prog1.isTerminated && !prog1.isWaiting then
        madeProgress |= prog1.executeInstruction(instructions(prog1.pc), prog0)
      madeProgress

    var iterations = 0
    while !isDeadlocked && iterations < 1000000 do
      if !runUntilBlocked then
        if prog0.isWaiting && !prog0.queueIsEmpty then prog0.executeInstruction(instructions(prog0.pc), prog1)
        if prog1.isWaiting && !prog1.queueIsEmpty then prog1.executeInstruction(instructions(prog1.pc), prog0)
      iterations += 1

    prog1.sendCount

end Day18