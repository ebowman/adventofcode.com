package y2015

import scala.util.parsing.combinator.RegexParsers

trait Day06 {

  case class Rect(left: Int, top: Int, width: Int, height: Int)

  trait Grid[T] {
    val grid: Array[Array[T]]

    def countOn(): Int

    def turnOn(rect: Rect): Grid[T]

    def turnOff(rect: Rect): Grid[T]

    def toggle(rect: Rect): Grid[T]

    def operate(range: Rect, write: (Int, Array[T]) => Unit): Grid[T] =
      for y <- range.top until range.top + range.height
          x <- range.left until range.left + range.width do
        write(x, grid(y))
      this
  }

  sealed trait Command {
    def range: Rect

    def operate(grid: Grid[_]): Grid[_]
  }

  case class TurnOn(range: Rect) extends Command {
    override def operate(grid: Grid[_]): Grid[_] = grid.turnOn(range)
  }

  case class TurnOff(range: Rect) extends Command {
    override def operate(grid: Grid[_]): Grid[_] = grid.turnOff(range)
  }

  case class Toggle(range: Rect) extends Command {
    override def operate(grid: Grid[_]): Grid[_] = grid.toggle(range)
  }

  class BitGrid(width: Int, height: Int) extends Grid[Boolean] {
    val grid: Array[Array[Boolean]] = Array.ofDim[Boolean](height, width)

    def countOn(): Int = grid.map(_.count(_ == true)).sum

    def turnOn(rect: Rect): Grid[Boolean] = operate(rect, (x, array) => array(x) = true)

    def turnOff(rect: Rect): Grid[Boolean] = operate(rect, (x, array) => array(x) = false)

    def toggle(rect: Rect): Grid[Boolean] = operate(rect, (x, array) => array(x) = !array(x))
  }

  class IntGrid(width: Int, height: Int) extends Grid[Int] {
    val grid: Array[Array[Int]] = Array.ofDim[Int](height, width)

    def countOn(): Int = grid.map(_.sum).sum

    def turnOn(rect: Rect): Grid[Int] = operate(rect, (x, array) => array(x) = array(x) + 1)

    def turnOff(rect: Rect): Grid[Int] = operate(rect, (x, array) => array(x) = Math.max(0, array(x) - 1))

    def toggle(rect: Rect): Grid[Int] = operate(rect, (x, array) => array(x) += 2)
  }

  def solve1(input: Seq[String]): Int =
    InstructionParser.compile(input, new BitGrid(1000, 1000)).countOn()

  def solve2(input: Seq[String]): Int =
    InstructionParser.compile(input, new IntGrid(1000, 1000)).countOn()

  object InstructionParser extends RegexParsers {
    override def skipWhitespace = false

    def num: Parser[Int] = """\d+""".r ^^ (_.toInt)

    def pair: Parser[(Int, Int)] = (num <~ ",") ~ num ^^ { case x ~ y => (x, y) }

    def range: Parser[Rect] = (pair <~ " through ") ~ pair ^^ {
      case tl ~ br => Rect(tl._1, tl._2, br._1 - tl._1 + 1, br._2 - tl._2 + 1)
    }

    def turnOn: Parser[TurnOn] = ("turn on" ~ " ") ~> range ^^ TurnOn.apply

    def turnOff: Parser[TurnOff] = ("turn off" ~ " ") ~> range ^^ TurnOff.apply

    def toggle: Parser[Toggle] = ("toggle" ~ " ") ~> range ^^ Toggle.apply

    def command: Parser[Command] = turnOn | turnOff | toggle

    def compile(input: Seq[String], grid: Grid[_]): Grid[_] =
      input.foreach(line => parseAll(command, line).get.operate(grid))
      grid
  }
}