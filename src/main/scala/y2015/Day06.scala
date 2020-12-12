package y2015

import scala.util.parsing.combinator.RegexParsers

object Lights {

  trait Grid {
    def countTurnedOn(): Int

    def turnOn(rect: Rect): Unit

    def turnOff(rect: Rect): Unit

    def toggle(rect: Rect): Unit
  }

  trait Command {
    def range: Rect

    def operate(grid: Grid): Grid
  }

  trait InstructionParser extends RegexParsers {
    override def skipWhitespace = false

    def num: Parser[Int] = """\d+""".r ^^ {
      _.toInt
    }

    def pair: Parser[(Int, Int)] = (num <~ ",") ~ num ^^ { case x ~ y => (x, y) }

    def range: Parser[Rect] = (pair <~ " through ") ~ pair ^^ { case tl ~ br => Rect.mkRect(tl._1, tl._2, br._1, br._2) }

    def turnOn: Parser[TurnOn] = ("turn on" ~ " ") ~> range ^^ {
      TurnOn.apply
    }

    def turnOff: Parser[TurnOff] = ("turn off" ~ " ") ~> range ^^ {
      TurnOff.apply
    }

    def toggle: Parser[Toggle] = ("toggle" ~ " ") ~> range ^^ {
      Toggle.apply
    }

    def command: Parser[Command] = turnOn | turnOff | toggle
  }

  case class Rect(left: Int, top: Int, width: Int, height: Int)

  class BitGrid(width: Int, height: Int) extends Grid {
    val grid = Array.ofDim[Boolean](height, width)

    def countTurnedOn(): Int = grid.map(_.count(_ == true)).sum

    def turnOn(rect: Rect): Unit = operate(rect, { case (x, array) => array(x) = true })

    def turnOff(rect: Rect): Unit = operate(rect, { case (x, array) => array(x) = false })

    def toggle(rect: Rect): Unit = operate(rect, { case (x, array) => array(x) = !array(x) })

    private def operate(range: Rect, write: (Int, Array[Boolean]) => Unit): Unit = {
      for (y <- range.top until range.top + range.height) {
        for (x <- range.left until range.left + range.width) {
          write(x, grid(y))
        }
      }
    }
  }

  class IntGrid(width: Int, height: Int) extends Grid {
    val grid = Array.ofDim[Int](height, width)

    def countTurnedOn(): Int = grid.map(_.sum).sum

    def turnOn(rect: Rect): Unit = operate(rect, { case (x, array) => array(x) = array(x) + 1 })

    private def operate(range: Rect, write: (Int, Array[Int]) => Unit): Unit = {
      for (y <- range.top until range.top + range.height) {
        for (x <- range.left until range.left + range.width) {
          write(x, grid(y))
        }
      }
    }

    def turnOff(rect: Rect): Unit = operate(rect, { case (x, array) => array(x) = Math.max(0, array(x) - 1) })

    def toggle(rect: Rect): Unit = operate(rect, { case (x, array) => array(x) = array(x) + 2 })
  }

  case class TurnOn(range: Rect) extends Command {
    override def operate(grid: Grid): Grid = {
      grid.turnOn(range); grid
    }
  }

  case class TurnOff(range: Rect) extends Command {
    override def operate(grid: Grid): Grid = {
      grid.turnOff(range); grid
    }
  }

  case class Toggle(range: Rect) extends Command {
    override def operate(grid: Grid): Grid = {
      grid.toggle(range); grid
    }
  }

  object Rect {
    def mkRect(x1: Int, y1: Int, x2: Int, y2: Int): Rect = Rect(x1, y1, x2 - x1 + 1, y2 - y1 + 1)
  }

}