package y2016

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.matching.Regex
import scala.util.Try
import scala.util.parsing.combinator.RegexParsers

trait Day23 {


  object Machine {
    val step = 3
    val toReg = Array("a", "b", "c", "d")

    def compile(code: Seq[String], a: Int): Machine = new Machine(Compiler.compile(code), mutable.Map("a" -> a).withDefaultValue(0))
  }

  class Machine(code: Array[Byte], val registers: mutable.Map[String, Int]) {

    import Machine._
    import Compiler.OpCode._

    var cursor = 0

    def load(c: Int): Int =
      if code(c) >= 100 then
        registers(s"${(code(c) - 100 + 'a').toChar}")
      else code(c).toInt

    def reg(c: Int): Option[String] =
      if code(c) >= 100 then
        Some(toReg(code(c) - 100))
      else None

    def hasNext(): Boolean =
      cursor <= code.length - step

    def next(): Unit = {
      // println(this)
      code(cursor) match {
        case CPY.code =>
          reg(cursor + 2).map(name => registers(name) = load(cursor + 1)).getOrElse(sys.error(""))
          cursor += step
        case INC.code =>
          reg(cursor + 1).map(name => registers(name) += 1).getOrElse(sys.error(""))
          cursor += step
        case DEC.code =>
          reg(cursor + 1).map(name => registers(name) -= 1).getOrElse(sys.error(""))
          cursor += step
        case JNZ.code =>
          if load(cursor + 1) == 0 then cursor += step
          else cursor += load(cursor + 2) * step
        case TGL.code =>
          val delta = cursor + load(cursor + 1) * step
          Try {
            code(delta) match {
              case CPY.code => code(delta) = JNZ.code
              case INC.code => code(delta) = DEC.code
              case DEC.code => code(delta) = INC.code
              case JNZ.code => code(delta) = CPY.code
              case TGL.code => code(delta) = INC.code
            }
          }
          cursor += step
      }
    }

    override def toString: String = s"Machine\n registers=$registers\n cursor=$cursor\n${decompile()}"

    def decompile(): String = {
      def pretty(b: Byte): String = if b >= 100 then s"${(b - 100 + 'a').toChar}" else s"${b.toInt}"

      val sb = new StringBuilder

      @tailrec def recurse(i: Int): Unit = {
        if i == cursor then sb.append("*") else sb.append(" ")
        if (i >= code.length - step) ()
        else
          val (r1, r2) = (pretty(code(i + 1)), pretty(code(i + 2)))
          code(i) match {
            case CPY.code => sb.append(s"cpy $r1 $r2\n")
            case INC.code => sb.append(s"inc $r1\n")
            case DEC.code => sb.append(s"dec $r1\n")
            case JNZ.code => sb.append(s"jnz $r1 $r2\n")
            case TGL.code => sb.append(s"tgl $r1\n")
          }
          recurse(i + step)
      }

      recurse(0)
      sb.toString()
    }
  }

  def solve1(input: Seq[String], a: Int): Int = {
    val machine = Machine.compile(input, a)
    println(machine)
    println("-" * 80)
    while (machine.hasNext()) machine.next()
    println(machine)
    machine.registers("a")
  }

  object Compiler extends RegexParsers {
    enum OpCode(val code: Byte):
      case CPY extends OpCode(1)
      case INC extends OpCode(2)
      case DEC extends OpCode(3)
      case JNZ extends OpCode(4)
      case TGL extends OpCode(5)

    import OpCode._

    def addressToByte(str: String): Byte = if str.head.isLetter then (str.head - 'a' + 100).toByte else str.toByte

    def address: Parser[Byte] = """-?\w+""".r ^^ addressToByte

    def cpy: Parser[Array[Byte]] = "cpy" ~> address ~ address ^^ {
      case src ~ dest => Array[Byte](CPY.code, src, dest)
    }

    def inc: Parser[Array[Byte]] = "inc" ~> address ^^ {
      dest => Array(INC.code, dest, 0)
    }

    def dec: Parser[Array[Byte]] = "dec" ~> address ^^ {
      dest => Array(DEC.code, dest, 0)
    }

    def jnz: Parser[Array[Byte]] = "jnz" ~> address ~ address ^^ {
      case src ~ dest => Array[Byte](JNZ.code, src, dest)
    }

    def tgl: Parser[Array[Byte]] = "tgl" ~> address ^^ {
      dest => Array[Byte](TGL.code, dest, 0)
    }

    def codeParser: Parser[Array[Byte]] = inc | dec | cpy | jnz | tgl

    def compile(code: Seq[String]): Array[Byte] = {
      code.foldLeft(Array[Byte]()) {
        case (compiled, line) => compiled ++ parseAll(codeParser, line).get
      }
    }
  }
}
