package y2016

import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.{max, min}

trait Day01 {
  def solve1(input: Seq[String]): Int = {
    val R = """R(\d+)""".r
    val L = """L(\d+)""".r
    case class Pos(x: Int = 0, y: Int = 0, dir: Int = 0) {
      def right: Pos = copy(dir = (dir + 1) % 4)

      def left: Pos = copy(dir = (dir + 3) % 4)

      def advance(n: Int): Pos = dir match {
        case 0 => copy(y = y + n)
        case 1 => copy(x = x + n)
        case 2 => copy(y = y - n)
        case 3 => copy(x = x - n)
      }

      def dist: Int = math.abs(x) + math.abs(y)

      @tailrec final def follow(instructions: Seq[String]): Pos = {
        if (instructions.isEmpty) this
        else {
          instructions.head match {
            case L(n) => left.advance(n.toInt).follow(instructions.tail)
            case R(n) => right.advance(n.toInt).follow(instructions.tail)
          }
        }
      }
    }
    Pos().follow(input).dist
  }

  def solve2(input: Seq[String]): Int = {
    val R = """R(\d+)""".r
    val L = """L(\d+)""".r
    case class Pos(x: Int = 0, y: Int = 0, dir: Int = 0) {
      def right: Pos = copy(dir = (dir + 1) % 4)

      def left: Pos = copy(dir = (dir + 3) % 4)

      def advance(n: Int): Pos = dir match {
        case 0 => copy(y = y + n)
        case 1 => copy(x = x + n)
        case 2 => copy(y = y - n)
        case 3 => copy(x = x - n)
      }

      def dist: Int = math.abs(x) + math.abs(y)

      def path(endPoint: Pos): Seq[(Int, Int)] =
        if (x == endPoint.x) {
          val r = if (y < endPoint.y) y + 1 until endPoint.y else endPoint.y until y
          for (i <- r) yield (x, i)
        } else {
          val r = if (x < endPoint.x) x + 1 to endPoint.x else endPoint.x until x
          for (i <- r) yield (i, y)
        }

      def follow(instructions: Seq[String], seen: Set[(Int, Int)] = Set((0, 0))): Pos = {
        val end = instructions.head match {
          case L(n) => left.advance(n.toInt)
          case R(n) => right.advance(n.toInt)
        }
        val p = this.path(end)
        p.find(seen).map(pt => Pos(pt._1, pt._2)).getOrElse(end.follow(instructions.tail, seen ++ p))
      }
    }
    Pos().follow(input).dist
  }
}
