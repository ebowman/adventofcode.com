package y2023

// see https://adventofcode.com/2023/day/12
trait Day12 {
  // Cache for memoization
  private val cache = scala.collection.mutable.HashMap[(String, List[Int], Int, Int, Int), Long]()

  def countArrangements(springs: String, groups: List[Int]): Long =
    cache.clear()
    solve(springs, groups, 0, 0, 0)

  def parseInput(line: String): (String, List[Int]) =
    val parts = line.split(" ")
    val springs = parts(0)
    val groups = parts(1).split(",").map(_.toInt).toList
    (springs, groups)

  def unfoldRecord(springs: String, groups: List[Int]): (String, List[Int]) =
    val unfoldedSprings = List.fill(5)(springs).mkString("?")
    val unfoldedGroups = List.fill(5)(groups).flatten
    (unfoldedSprings, unfoldedGroups)

  def solvePart1(input: Seq[String]): Long =
    input.map { line =>
      val (springs, groups) = parseInput(line)
      countArrangements(springs, groups)
    }.sum

  def solvePart2(input: Seq[String]): Long =
    input.map { line =>
      val (springs, groups) = parseInput(line)
      val (unfoldedSprings, unfoldedGroups) = unfoldRecord(springs, groups)
      countArrangements(unfoldedSprings, unfoldedGroups)
    }.sum

  private def solve(springs: String,
                    groups: List[Int],
                    pos: Int,
                    groupIndex: Int,
                    currentGroupLen: Int): Long = {
    val key = (springs.substring(pos), groups.drop(groupIndex), pos, groupIndex, currentGroupLen)
    if cache.contains(key) then cache(key)
    else {
      if (pos == springs.length) { // If we've reached the end of the string
        val isValid =
          (groupIndex == groups.length && currentGroupLen == 0) ||
            (groupIndex == groups.length - 1 && currentGroupLen == groups.last)
        if isValid then 1L else 0L
      } else {
        val possible = springs(pos) match {
          case '?' => List('.', '#')
          case c => List(c)
        }
        val result = possible.map {
          case '.' =>
            if currentGroupLen == 0 then solve(springs, groups, pos + 1, groupIndex, 0) // Not in a group, continue
            else if groupIndex < groups.length && currentGroupLen == groups(groupIndex) // Finished a correct group
            then solve(springs, groups, pos + 1, groupIndex + 1, 0)
            else 0L // Invalid group length

          case '#' =>
            if groupIndex >= groups.length || (currentGroupLen > 0 && currentGroupLen >= groups(groupIndex))
            then 0L // Too many springs in group
            else solve(springs, groups, pos + 1, groupIndex, currentGroupLen + 1) // Continue or start group
        }.sum

        cache(key) = result
        result
      }
    }
  }
}