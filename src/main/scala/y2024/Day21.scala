package y2024

class Day21 extends util.Day(21):
  private val numPadLines = Seq("789", "456", "123", ".0A")
  private val dirPadLines = Seq(".^A", "<v>")
  private val (numToButton, numToCoord) = createKeypadMappings(numPadLines)
  private val (dirToButton, dirToCoord) = createKeypadMappings(dirPadLines)
  private val numericPad = Keypad(numToButton, numToCoord)
  private val directionalPad = Keypad(dirToButton, dirToCoord)

  def solvePart1(input: IndexedSeq[String]): Long = solve(input, expansions = 2)

  def solvePart2(input: IndexedSeq[String]): Long = solve(input, expansions = 25)

  private def solve(input: IndexedSeq[String], expansions: Int): Long =
    input.map: line =>
      val numericValue = line.init.toInt
      val numRoute = computeRoute(line, numericPad)
      val finalRouteMap = (1 to expansions).foldLeft(Map(numRoute -> 1L)): (routeMap, _) =>
        expandRoute(routeMap, directionalPad)
      routeLen(finalRouteMap) * numericValue
    .sum

  private def computeRoute(path: String, keypad: Keypad): String =
    path.foldLeft(("", 'A')):
      case ((acc, start), ch) =>
        val stepStr = step(start, ch, keypad)
        (acc + stepStr, ch)
    ._1

  private def routeLen(routeMap: Map[String, Long]): Long =
    routeMap.foldLeft(0L):
      case (acc, (route, count)) =>
        acc + route.length.toLong * count

  private def expandRoute(routeMap: Map[String, Long], keypad: Keypad): Map[String, Long] =
    val expandedRoutes = for {
      (subRoute, qty) <- routeMap.toSeq
      (piece, pieceCount) <- computeRoutes2(subRoute, keypad)
    } yield (piece, pieceCount * qty)

    expandedRoutes.groupMapReduce(_._1)(_._2)(_ + _)

  private def computeRoutes2(path: String, keypad: Keypad): Map[String, Long] =
    path.foldLeft(Map.empty[String, Long].withDefaultValue(0L), 'A'):
      case ((counts, current), ch) =>
        val stepStr = step(current, ch, keypad)
        (counts.updated(stepStr, counts(stepStr) + 1), ch)
    ._1

  private def step(source: Char, target: Char, keypad: Keypad): String =
    val (startRow, startCol) = keypad.toCoord(source)
    val (targetRow, targetCol) = keypad.toCoord(target)
    val rowDiff = targetRow - startRow
    val colDiff = targetCol - startCol

    val verticalMovement = if rowDiff > 0 then "v" * rowDiff else "^" * (-rowDiff)
    val horizontalMovement = if colDiff > 0 then ">" * colDiff else "<" * (-colDiff)

    def canAim(row: Int, col: Int): Boolean = keypad.toButton.contains((row, col))

    if colDiff > 0 && canAim(targetRow, startCol) then verticalMovement + horizontalMovement + "A"
    else if canAim(startRow, targetCol) then horizontalMovement + verticalMovement + "A"
    else verticalMovement + horizontalMovement + "A"

  private def createKeypadMappings(lines: Seq[String]): (Map[(Int, Int), Char], Map[Char, (Int, Int)]) =
    val coordsAndChars = for
      (line, row) <- lines.zipWithIndex
      (char, col) <- line.zipWithIndex
      if char != '.'
    yield ((row, col), char)

    val toButton = coordsAndChars.toMap
    val toCoord = coordsAndChars.map:
      case ((r, c), ch) =>
        ch -> (r, c)
    .toMap
    (toButton, toCoord)

  private case class Keypad(toButton: Map[(Int, Int), Char], toCoord: Map[Char, (Int, Int)])
end Day21
