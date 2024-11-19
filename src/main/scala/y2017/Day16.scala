package y2017

import scala.annotation.tailrec

trait Day16:
  private enum DanceMove:
    case Spin(x: Int)
    case Exchange(a: Int, b: Int)
    case Partner(a: Char, b: Char)

  private def parseMove(s: String): DanceMove =
    s.head match
      case 's' => DanceMove.Spin(s.tail.toInt)
      case 'x' =>
        val Array(a, b) = s.tail.split("/").map(_.toInt)
        DanceMove.Exchange(a, b)
      case 'p' =>
        val Array(a, b) = s.tail.split("/").map(_.head)
        DanceMove.Partner(a, b)

  private def parseMoves(input: Seq[String]): Seq[DanceMove] =
    input.head.split(",").map(parseMove).toSeq

  private def applyMove(programs: Vector[Char], move: DanceMove): Vector[Char] =
    move match
      case DanceMove.Spin(x) =>
        val (end, start) = programs.splitAt(programs.length - x)
        start ++ end
      case DanceMove.Exchange(a, b) =>
        programs.updated(a, programs(b)).updated(b, programs(a))
      case DanceMove.Partner(a, b) =>
        val posA = programs.indexOf(a)
        val posB = programs.indexOf(b)
        programs.updated(posA, b).updated(posB, a)

  private def dance(programs: Vector[Char], moves: Seq[DanceMove]): Vector[Char] =
    moves.foldLeft(programs)(applyMove)

  private def findCycle(initial: Vector[Char], moves: Seq[DanceMove]): (Int, Vector[Vector[Char]]) =
    @tailrec
    def recurse(current: Vector[Char], seen: Vector[Vector[Char]]): (Int, Vector[Vector[Char]]) =
      val next = dance(current, moves)
      val cycleStart = seen.indexOf(next)
      if cycleStart >= 0 then
        (cycleStart, seen)
      else
        recurse(next, seen :+ next)

    recurse(initial, Vector(initial))

  def solvePart1(input: Seq[String], size: Int = 16): String =
    val initialPrograms = ('a' to ('a' + size - 1).toChar).toVector
    val moves = parseMoves(input)
    dance(initialPrograms, moves).mkString

  def solvePart2(input: Seq[String], size: Int = 16): String =
    val initialPrograms = ('a' to ('a' + size - 1).toChar).toVector
    val moves = parseMoves(input)
    val iterations = 1_000_000_000

    val (cycleStart, states) = findCycle(initialPrograms, moves)
    val cycleLength = states.length - cycleStart
    val finalStateIndex = cycleStart + ((iterations - cycleStart) % cycleLength)

    states(finalStateIndex).mkString
end Day16