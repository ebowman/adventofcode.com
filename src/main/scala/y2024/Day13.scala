package y2024

// see https://adventofcode.com/2024/day/13
class Day13 extends util.Day(13):

  def solvePart1(input: IndexedSeq[String]): Any =
    val machines = parseMachines(input)
    val solutions = machines.flatMap(_.solveMachine)
    solutions.sum

  def solvePart2(input: IndexedSeq[String]): Any =
    val offset = 10_000_000_000_000L
    val machines = parseMachines(input).map(_.offset(offset))
    val solutions = machines.flatMap(_.solveMachine)
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


  case class Machine(xa: Int, ya: Int, xb: Int, yb: Int, x: Long, y: Long):
    def offset(offset: Long): Machine = copy(x = x + offset, y = y + offset)

    def solveMachine: Option[Long] =
      // great discussion: https://www.reddit.com/r/adventofcode/comments/1hd7irq/2024_day_13_an_explanation_of_the_mathematics/
      val det = xb.toLong * ya.toLong - yb.toLong * xa.toLong
      if det == 0 then None
      else
        val aNum = xb.toLong * y - yb.toLong * x
        val bNum = x - (aNum / det) * xa
        if aNum % det != 0 then None
        else if bNum % xb.toLong != 0 then None
        else Some(3 * aNum / det + bNum / xb)

end Day13
