package y2015

trait Day03 {

  val dirs = Map('^' -> (0, -1), '<' -> (-1, 0), '>' -> (1, 0), 'v' -> (0, 1))

  def iter(dir: Char, pos: (Int, Int)): (Int, Int) = (pos._1 + dirs(dir)._1, pos._2 + dirs(dir)._2)

  def solve1(str: String): Int =
    val (_, positions) = str.foldLeft(((0, 0), Set((0, 0)))) {
      case ((curPos, visited), dir) =>
        val newPos = iter(dir, curPos)
        (newPos, visited + newPos)
    }
    positions.size

  def solve2(str: String): Int =
    val (start, empty) = ((0, 0), Set((0, 0)))
    val (_, _, robotSet, santaSet) = str.grouped(2).foldLeft((start, start, empty, empty)) {
      case ((robotPos, santaPos, robotSet, santaSet), instructions) =>
        val newRobotPos = iter(instructions(0), robotPos)
        val newSantaPos = iter(instructions(1), santaPos)
        (newRobotPos, newSantaPos, robotSet + newRobotPos, santaSet + newSantaPos)
    }
    (robotSet ++ santaSet).size
}
