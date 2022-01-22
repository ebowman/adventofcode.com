package y2020

import scala.annotation.tailrec
import scala.util.matching.Regex

trait Day12 {

  trait Ship {
    val row: Int
    val col: Int
    val dir: (Int, Int)

    val Ins: Regex = """([A-Z])([0-9]+)""".r

    @tailrec
    final def left(d: (Int, Int), n: Int): (Int, Int) = if n == 0 then d else left((-d._2, d._1), n - 1)

    @tailrec
    final def right(d: (Int, Int), n: Int): (Int, Int) = if n == 0 then d else right((d._2, -d._1), n - 1)

    def manhattanDistance: Int = math.abs(row) + math.abs(col)

    @tailrec
    final def evolve(ins: Iterable[String], s: Ship = this): Ship =
      if ins.isEmpty then s else evolve(ins.tail, s.next(ins.head))

    def next(direction: String): Ship
  }

  case class Ship1(row: Int = 0, col: Int = 0, dir: (Int, Int) = (0, 1)) extends Ship {

    def next(direction: String): Ship = {
      direction match {
        case Ins("L", num) => copy(dir = left(dir, num.toInt / 90))
        case Ins("R", num) => copy(dir = right(dir, num.toInt / 90))
        case Ins("F", num) => copy(row = row + num.toInt * dir._1, col = col + num.toInt * dir._2)
        case Ins("N", num) => copy(row = row - num.toInt)
        case Ins("S", num) => copy(row = row + num.toInt)
        case Ins("E", num) => copy(col = col + num.toInt)
        case Ins("W", num) => copy(row = row - num.toInt)
      }
    }
  }

  case class Ship2(row: Int = 0, col: Int = 0, dir: (Int, Int) = (-1, 10)) extends Ship {

    def next(direction: String): Ship2 = {
      direction match {
        case Ins("L", num) => copy(dir = left(dir, num.toInt / 90))
        case Ins("R", num) => copy(dir = right(dir, num.toInt / 90))
        case Ins("F", num) => copy(row = row + num.toInt * dir._1, col = col + num.toInt * dir._2)
        case Ins("N", num) => copy(dir = (dir._1 - num.toInt, dir._2))
        case Ins("S", num) => copy(dir = (dir._1 + num.toInt, dir._2))
        case Ins("E", num) => copy(dir = (dir._1, dir._2 + num.toInt))
        case Ins("W", num) => copy(dir = (dir._1, dir._2 - num.toInt))
      }
    }
  }

}