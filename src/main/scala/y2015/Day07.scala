package y2015

import scala.util.parsing.combinator.RegexParsers

trait Day07 {

  def compile(input: Seq[String]): Map[String, Signal] = {
    val compiler = new Compiler
    input.foreach(line => compiler.parseAll(compiler.parser, line))
    compiler.wires.toMap
  }

  sealed trait Signal {
    def voltage: Int
  }

  class Compiler extends RegexParsers {

    import scala.collection.mutable

    val wires = new mutable.HashMap[String, Signal]

    def parser: Parser[Unit] = wire | wire1 | and | and1 | or | lshift | rshift | _not

    def wire: Parser[Unit] = (num <~ "->") ~ name ^^ { case num ~ name => wires.put(name, Wire(num)) }

    def wire1: Parser[Unit] = (name <~ "->") ~ name ^^ { case s ~ name => wires.put(name, Named(s)) }

    def and: Parser[Unit] = (name <~ "AND") ~ name ~ ("->" ~> name) ^^ {
      case s1 ~ s2 ~ d => wires.put(d, OpAnd(Named(s1), Named(s2)))
    }

    def and1: Parser[Unit] = (num <~ "AND") ~ name ~ ("->" ~> name) ^^ {
      case n ~ s ~ d => wires.put(d, OpAnd(Wire(n), Named(s)))
    }

    def or: Parser[Unit] = (name <~ "OR") ~ name ~ ("->" ~> name) ^^ {
      case s1 ~ s2 ~ d => wires.put(d, OpOr(Named(s1), Named(s2)))
    }

    def lshift: Parser[Unit] = name ~ ("LSHIFT" ~> num) ~ ("->" ~> name) ^^ {
      case s ~ shift ~ d => wires.put(d, OpLShift(Named(s), shift))
    }

    def rshift: Parser[Unit] = name ~ ("RSHIFT" ~> num) ~ ("->" ~> name) ^^ {
      case s ~ shift ~ d => wires.put(d, OpRShift(Named(s), shift))
    }

    def num: Parser[Int] = """\d+""".r ^^ (_.toInt)

    def name: Parser[String] = """[a-z]+""".r

    def _not: Parser[Unit] = ("NOT" ~> name) ~ ("->" ~> name) ^^ {
      case s ~ d => wires.put(d, OpNot(Named(s))) }

    case class Named(name: String) extends Signal {
      lazy val voltage: Int = wires(name).voltage
    }

    case class Wire(override val voltage: Int) extends Signal

    case class OpAnd(s1: Signal, s2: Signal) extends Signal {
      override lazy val voltage: Int = s1.voltage & s2.voltage
    }

    case class OpOr(s1: Signal, s2: Signal) extends Signal {
      override lazy val voltage: Int = s1.voltage | s2.voltage
    }

    case class OpLShift(s: Signal, shift: Int) extends Signal {
      override lazy val voltage: Int = (s.voltage << shift) & 0xFFFF
    }

    case class OpRShift(s: Signal, shift: Int) extends Signal {
      override lazy val voltage: Int = s.voltage >> shift
    }

    case class OpNot(s: Signal) extends Signal {
      override lazy val voltage: Int = (~s.voltage) & 0xFFFF
    }
  }
}