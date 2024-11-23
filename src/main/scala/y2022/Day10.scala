package y2022

trait Day10:
  enum Instruction:
    case Noop
    case AddX(value: Int)

  private def parseInstruction(line: String): Instruction =
    line match
      case "noop" => Instruction.Noop
      case s"addx $value" => Instruction.AddX(value.toInt)

  private def processInstructions(instructions: Seq[String]): LazyList[Int] =
    val initialState = LazyList(1)

    instructions.foldLeft(initialState): (acc, instruction) =>
      val currentX = acc.last
      parseInstruction(instruction) match
        case Instruction.Noop =>
          acc.appended(currentX)
        case Instruction.AddX(value) =>
          acc.appended(currentX).appended(currentX + value)

  def solvePart1(input: Seq[String]): Int =
    val registerValues = processInstructions(input)
      .zipWithIndex.map((value, idx) => (idx + 1, value))
    val signalStrengths = for
      cycle <- List(20, 60, 100, 140, 180, 220)
      value = registerValues(cycle - 1)
    yield cycle * value._2

    signalStrengths.sum

  def solvePart2(input: Seq[String]): String =
    val registerValues = processInstructions(input)
      .take(240)
    val screen = Array.ofDim[Char](6, 40)

    for
      cycle <- 0 until 240
      row = cycle / 40
      col = cycle % 40
      spritePosition = registerValues(cycle)
      pixel = if col >= spritePosition - 1 && col <= spritePosition + 1 then '#' else '.'
    do
      screen(row)(col) = pixel

    screen.map(_.mkString).mkString("\n")

end Day10