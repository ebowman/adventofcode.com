package y2023

// see https://adventofcode.com/2023/day/13
trait Day13 {
  def solvePart1(input: Seq[String]): Int =
    val patterns = parseInput(input.mkString("\n"))
    findReflectionValue(patterns)

  def solvePart2(input: Seq[String]): Int =
    val patterns = parseInput(input.mkString("\n"))
    findReflectionValue(patterns, findAlternate = true)

  def findReflectionValue(patterns: List[List[String]], findAlternate: Boolean = false): Int =
    patterns.map { pattern =>
      if !findAlternate then
        // Part 1 logic
        val horizontalValue = findHorizontalReflection(pattern, None)
        horizontalValue.getOrElse(findVerticalReflection(pattern, None).getOrElse(0))
      else
        // Part 2 logic - find reflection after fixing one smudge
        findAlternateReflection(pattern)
    }.sum

  def findAlternateReflection(pattern: List[String]): Int = {
    import scala.util.boundary

    // Find original reflection value to exclude it
    val originalHorizontal = findHorizontalReflection(pattern, None)
    val originalVertical = findVerticalReflection(pattern, None)
    val originalValue = originalHorizontal.getOrElse(originalVertical.getOrElse(0))

    boundary {
      // Try each possible smudge position
      val allPositions = for {
        row <- pattern.indices
        col <- pattern.head.indices
      } yield (row, col)

      // Process each position and break when we find a valid reflection
      allPositions.foreach { case (row, col) =>
        // Create new pattern with flipped character at current position
        val newPattern = pattern.updated(row,
          pattern(row).updated(col, if (pattern(row)(col) == '#') '.' else '#')
        )

        // Look for new reflection line, excluding the original
        val newHorizontal = findHorizontalReflection(newPattern, Some(originalValue))
        if (newHorizontal.isDefined && newHorizontal.get != originalValue) {
          boundary.break(newHorizontal.get)
        }

        val newVertical = findVerticalReflection(newPattern, Some(originalValue))
        if (newVertical.isDefined && newVertical.get != originalValue) {
          boundary.break(newVertical.get)
        }
      }
      0 // Return 0 if no alternate reflection found
    }
  }

  def findHorizontalReflection(pattern: List[String], excludeValue: Option[Int]): Option[Int] =
    val rows = pattern

    def isReflectionAt(idx: Int): Boolean = {
      val (upper, lower) = rows.splitAt(idx)
      val minSize = math.min(upper.length, lower.length)
      upper.reverse.take(minSize) == lower.take(minSize)
    }

    (1 until rows.length)
      .find(idx => isReflectionAt(idx) && !excludeValue.contains(idx * 100))
      .map(_ * 100)

  def findVerticalReflection(pattern: List[String], excludeValue: Option[Int]): Option[Int] =
    val cols = pattern.head.indices.map(i => pattern.map(_(i)).mkString).toList

    def isReflectionAt(idx: Int): Boolean =
      val (left, right) = cols.splitAt(idx)
      val minSize = math.min(left.length, right.length)
      left.reverse.take(minSize) == right.take(minSize)

    (1 until cols.length)
      .find(idx => isReflectionAt(idx) && !excludeValue.contains(idx))
      .map(identity)

  def parseInput(input: String): List[List[String]] =
    input.trim.split("\n\n").map(pattern =>
      pattern.split("\n").toList.filter(_.nonEmpty)
    ).toList
}