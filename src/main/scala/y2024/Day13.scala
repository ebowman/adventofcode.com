package y2024

// see https://adventofcode.com/2024/day/13
class Day13 extends util.Day(13):

  def solvePart1(input: IndexedSeq[String]): Any =
    val machines = parseMachines(input)
    val solutions = machines.flatMap(solveMachine)
    solutions.sum

  def solvePart2(input: IndexedSeq[String]): Any =
    val offset = 10000000000000L
    val machines = parseMachines(input).map(m => m.copy(x = m.x + offset, y = m.y + offset))
    val solutions = machines.flatMap(solveMachine)
    solutions.sum

  private def parseMachines(input: IndexedSeq[String]): IndexedSeq[Machine] =
    input
      .filterNot(_.trim.isEmpty)
      .grouped(3)
      .map: lines =>
        val (xa, ya) = parseLine(lines(0))
        val (xb, yb) = parseLine(lines(1))
        val (x, y) = parseLine(lines(2))
        Machine(xa.toInt, ya.toInt, xb.toInt, yb.toInt, x, y)
      .toIndexedSeq

  private def parseLine(line: String): (Long, Long) =
    val parts = line.split(",").map(_.trim)
    val xPart = parts(0).split("X")(1).trim
    val xv = xPart.replace("=", "").toLong
    val yPart = parts(1).split("Y")(1).trim
    val yv = yPart.replace("=", "").toLong
    (xv, yv)

  private def solveMachine(m: Machine): Option[Long] =
    // see https://www.math.uwaterloo.ca/~wgilbert/Research/GilbertPathria.pdf
    val det = m.xb.toLong * m.ya.toLong - m.yb.toLong * m.xa.toLong
    if det == 0 then None
    else
      val aNum = m.xb.toLong * m.y - m.yb.toLong * m.x
      if aNum % det != 0 then None
      else
        val a = aNum / det
        val bNum = m.x - a * m.xa
        if m.xb == 0 then None
        else if bNum % m.xb.toLong != 0 then None
        else
          val b = bNum / m.xb
          if a >= 0 && b >= 0 then Some(3 * a + b) else None

  case class Machine(xa: Int, ya: Int, xb: Int, yb: Int, x: Long, y: Long)

end Day13
