package y2017

trait Day08:
  private case class Instruction(
                                  register: String,
                                  operation: String,
                                  amount: Int,
                                  condRegister: String,
                                  condOp: String,
                                  condValue: Int
                                )

  private def parseInstruction(line: String): Instruction =
    line.split(" ") match
      case Array(reg, op, amt, "if", condReg, condOp, condVal) =>
        Instruction(
          reg,
          op,
          amt.toInt,
          condReg,
          condOp,
          condVal.toInt
        )

  private def evaluateCondition(
                                 registers: Map[String, Int],
                                 register: String,
                                 op: String,
                                 value: Int
                               ): Boolean =
    val regValue = registers.getOrElse(register, 0)
    op match
      case ">" => regValue > value
      case "<" => regValue < value
      case ">=" => regValue >= value
      case "<=" => regValue <= value
      case "==" => regValue == value
      case "!=" => regValue != value

  private def processInstructions(input: Seq[String]): (Int, Int) =
    val instructions = input.map(parseInstruction)

    def processInstruction(
                            registers: Map[String, Int],
                            maxEver: Int,
                            instr: Instruction
                          ): (Map[String, Int], Int) =
      if evaluateCondition(registers, instr.condRegister, instr.condOp, instr.condValue) then
        val currentValue = registers.getOrElse(instr.register, 0)
        val delta = if instr.operation == "inc" then instr.amount else -instr.amount
        val newValue = currentValue + delta
        (registers + (instr.register -> newValue), maxEver.max(newValue))
      else
        (registers, maxEver)

    val (finalRegisters, maxEverSeen) = instructions.foldLeft((Map[String, Int](), 0)) {
      case ((regs, maxEver), instr) => processInstruction(regs, maxEver, instr)
    }

    (finalRegisters.values.max, maxEverSeen)

  def solvePart1(input: Seq[String]): Int =
    processInstructions(input)._1

  def solvePart2(input: Seq[String]): Int =
    processInstructions(input)._2

end Day08