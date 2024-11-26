package y2022

trait Day05:
  case class Move(count: Int, from: Int, to: Int)

  // Parse the initial state of stacks
  private def parseStacks(input: Seq[String]): Vector[List[Char]] =
    val stackLines = input.takeWhile(_.contains('['))
    val numberLine = input(stackLines.length)
    val numStacks = numberLine.trim.split("\\s+").length

    // Initialize empty stacks
    val stacks = Vector.fill(numStacks)(List[Char]())

    // Fill stacks from bottom to top
    stackLines.foldRight(stacks): (line, acc) =>
      acc.zipWithIndex.map: (stack, i) =>
        val charPos = 1 + (i * 4)
        if charPos < line.length && line(charPos) != ' ' then
          line(charPos) :: stack
        else
          stack

  // Parse move instructions
  private def parseMoves(input: Seq[String]): Seq[Move] =
    val movePattern = """move (\d+) from (\d+) to (\d+)""".r
    input
      .dropWhile(!_.startsWith("move"))
      .map:
        case movePattern(count, from, to) =>
          Move(count.toInt, from.toInt - 1, to.toInt - 1)

  // Execute moves for part 1 (moving one crate at a time)
  private def executeMovesPart1(stacks: Vector[List[Char]], moves: Seq[Move]): Vector[List[Char]] =
    moves.foldLeft(stacks): (currentStacks, move) =>
      (1 to move.count).foldLeft(currentStacks): (stacks, _) =>
        if stacks(move.from).isEmpty then stacks
        else
          val crate :: remaining = stacks(move.from): @unchecked
          stacks
            .updated(move.from, remaining)
            .updated(move.to, crate :: stacks(move.to))

  // Execute moves for part 2 (moving multiple crates at once)
  private def executeMovesPart2(stacks: Vector[List[Char]], moves: Seq[Move]): Vector[List[Char]] =
    moves.foldLeft(stacks): (currentStacks, move) =>
      val (movingCrates, remainingStack) = currentStacks(move.from).splitAt(
        math.min(move.count, currentStacks(move.from).length)
      )
      currentStacks
        .updated(move.from, remainingStack)
        .updated(move.to, movingCrates ::: currentStacks(move.to))

  // Get top crates as a String
  private def getTopCrates(stacks: Vector[List[Char]]): String =
    stacks.map(_.headOption.getOrElse(' ')).mkString

  def solvePart1(input: Seq[String]): String =
    val stacks = parseStacks(input)
    val moves = parseMoves(input)
    val finalStacks = executeMovesPart1(stacks, moves)
    getTopCrates(finalStacks)

  def solvePart2(input: Seq[String]): String =
    val stacks = parseStacks(input)
    val moves = parseMoves(input)
    val finalStacks = executeMovesPart2(stacks, moves)
    getTopCrates(finalStacks)
end Day05