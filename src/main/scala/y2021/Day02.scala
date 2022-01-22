package y2021

import scala.annotation.tailrec

trait Day02 {
  val Forward = """forward (\d+)""".r
  val Up = """up (\d+)""".r
  val Down = """down (\d+)""".r

  case class Ship(pos: Int = 0, depth: Int = 0) {
    def next(cmd: String): Ship =
      cmd match {
        case Forward(d) => copy(pos = pos + d.toInt)
        case Up(d) => copy(depth = depth - d.toInt)
        case Down(d) => copy(depth = depth + d.toInt)
      }
  }

  case class Ship2(pos: Int = 0, depth: Int = 0, aim: Int = 0) {
    def next(cmd: String): Ship2 =
      cmd match {
        case Forward(d) => copy(pos = pos + d.toInt, depth = depth + aim * d.toInt)
        case Up(d) => copy(aim = aim - d.toInt)
        case Down(d) => copy(aim = aim + d.toInt)
      }
  }

  def solve1(ins: Seq[String]): Int = {
    @tailrec def recurse(ship: Ship, ins: Seq[String]): Ship =
      if ins.isEmpty then ship else recurse(ship.next(ins.head), ins.tail)
    val r = recurse(Ship(), ins)
    r.pos * r.depth
  }

  def solve2(ins: Seq[String]): Int = {
    @tailrec def recurse(ship: Ship2, ins: Seq[String]): Ship2 =
      if ins.isEmpty then ship else recurse(ship.next(ins.head), ins.tail)
    val r = recurse(Ship2(), ins)
    r.pos * r.depth
  }

}