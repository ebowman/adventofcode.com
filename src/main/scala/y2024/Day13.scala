package y2024

// see https://adventofcode.com/2024/day/13
class Day13 extends util.Day(13):

  def solvePart1(input: IndexedSeq[String]): Long =
    val machines = parseMachines(input)
    val solutions = machines.flatMap(_.solveMachine)
    solutions.sum

  def solvePart2(input: IndexedSeq[String]): Long =
    val offset = 10_000_000_000_000L
    val machines = parseMachines(input).map(_.offset(offset))
    val solutions = machines.flatMap(_.solveMachine)
    solutions.sum

  private def parseMachines(input: IndexedSeq[String]): IndexedSeq[Machine] =
    input
      .filterNot(_.isEmpty)
      .grouped(3)
      .map: lines =>
        val (xa, ya) = parseLine(lines(0))
        val (xb, yb) = parseLine(lines(1))
        val (x, y) = parseLine(lines(2))
        Machine(xa, ya, xb, yb, x, y)
      .toIndexedSeq
  end parseMachines

  private def parseLine(line: String): (Int, Int) =
    val parts = line.split(",")
    val xPart = parts(0).split("X")(1)
    val xv = xPart.replace("=", "").toInt
    val yPart = parts(1).split("Y")(1)
    val yv = yPart.replace("=", "").toInt
    (xv, yv)
  end parseLine

  case class Machine(velocityX1: Int, velocityY1: Int, velocityX2: Int, velocityY2: Int, targetX: Long, targetY: Long):
    def offset(offset: Long): Machine = copy(targetX = targetX + offset, targetY = targetY + offset)

    def solveMachine: Option[Long] =
      /*
      
       We need to solve:
       timeA * velocityX1 + timeB * velocityX2 = targetX  (equation 1)
       timeA * velocityY1 + timeB * velocityY2 = targetY  (equation 2)

       In matrix form this is:
       | velocityX1  velocityX2 | | timeA | = | targetX |
       | velocityY1  velocityY2 | | timeB |   | targetY |

       The determinant = (velocityX2 * velocityY1 - velocityY2 * velocityX1) tells us:
       - If det = 0: The matrix is singular (no unique solution exists)
         This means the button movements are parallel/linearly dependent,
         and we can never reach the target
       - If det ≠ 0: We can solve for timeA and timeB using Cramer's rule:
         timeA = (velocityX2 * targetY - velocityY2 * targetX) / determinant
         timeB = (targetX * velocityY1 - targetY * velocityX1) / determinant

       The numerators in these calculations represent distances in the coordinate system.
       We then check if these distances are evenly divisible by our movement rates
       to ensure we can reach the target with whole number button presses.

       */
      val determinant = velocityX2 * velocityY1 - velocityY2 * velocityX1

      if determinant == 0 then
        None // Matrix is singular - buttons movements are parallel/dependent
      else
        // Using Cramer's rule to solve for timeA first
        val distanceA = velocityX2 * targetY - velocityY2 * targetX

        // Check if this distance is evenly divisible by our movement rate
        if distanceA % determinant != 0 then
          None // No integer solution for number of A button presses
        else
          val timeA = distanceA / determinant

          // Calculate remaining distance to target after A moves
          val remainingDistance = targetX - timeA * velocityX1

          // Check if remaining distance is evenly divisible by B's movement rate
          if remainingDistance % velocityX2 != 0 then
            None // No integer solution for number of B button presses
          else
            val timeB = remainingDistance / velocityX2
            // Cost is 3 tokens per A press plus 1 token per B press
            Some(3 * timeA + timeB)
    end solveMachine
end Day13
