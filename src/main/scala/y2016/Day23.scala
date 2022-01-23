package y2016

import scala.collection.mutable
import scala.util.Try
import scala.util.parsing.combinator.RegexParsers

trait Day23 {

  import Compiler.OpCode._

  val step = 3
  val toReg = Array("a", "b", "c", "d")
  val toggles = Map(CPY -> JNZ, INC -> DEC, DEC -> INC, JNZ -> CPY, TGL -> INC).map(x => x._1.code -> x._2.code)

  def solve(input: Seq[String], a: Int): Int =
    new Machine(Compiler.compile(input), mutable.Map("a" -> a).withDefaultValue(0)).execute()

  class Machine(code: Array[Byte], val registers: mutable.Map[String, Int]) {
    var cursor = 0

    def load(c: Int): Int = if code(c) >= 100 then registers(s"${(code(c) - 100 + 'a').toChar}") else code(c).toInt

    def reg(c: Int): Option[String] = if code(c) >= 100 then Some(toReg(code(c) - 100)) else None

    def hasNext(): Boolean = cursor <= code.length - step

    def next(): Unit =
      code(cursor) match {
        case CPY.code =>
          if code(cursor + step) == INC.code &&
            code(cursor + 2 * step) == DEC.code &&
            code(cursor + 3 * step) == JNZ.code &&
            code(cursor + 4 * step) == DEC.code &&
            code(cursor + 5 * step) == JNZ.code
          then
            reg(cursor + step + 1).foreach(dest => registers(dest) += load(cursor + 1) * load(cursor + 4 * step + 1))
            cursor += 6 * step
          else
            reg(cursor + 2).foreach(name => registers(name) = load(cursor + 1))
            cursor += step
        case INC.code =>
          reg(cursor + 1).foreach(name => registers(name) += 1)
          cursor += step
        case DEC.code =>
          reg(cursor + 1).foreach(name => registers(name) -= 1)
          cursor += step
        case JNZ.code =>
          cursor += (if load(cursor + 1) == 0 then step else load(cursor + 2) * step)
        case TGL.code =>
          val delta = cursor + load(cursor + 1) * step
          Try(code(delta) = toggles(code(delta)))
          cursor += step
      }

    def execute(): Int =
      while (hasNext()) next()
      registers("a")
  }

  object Compiler extends RegexParsers {
    enum OpCode(val code: Byte):
      case CPY extends OpCode(1)
      case INC extends OpCode(2)
      case DEC extends OpCode(3)
      case JNZ extends OpCode(4)
      case TGL extends OpCode(5)

    def addressToByte(str: String): Byte = if str.head.isLetter then (str.head - 'a' + 100).toByte else str.toByte

    def address: Parser[Byte] = """-?\w+""".r ^^ addressToByte

    def cpy: Parser[Array[Byte]] = "cpy" ~> address ~ address ^^ { case src ~ dest => Array[Byte](CPY.code, src, dest) }

    def inc: Parser[Array[Byte]] = "inc" ~> address ^^ { dest => Array(INC.code, dest, 0) }

    def dec: Parser[Array[Byte]] = "dec" ~> address ^^ { dest => Array(DEC.code, dest, 0) }

    def jnz: Parser[Array[Byte]] = "jnz" ~> address ~ address ^^ { case src ~ dest => Array[Byte](JNZ.code, src, dest) }

    def tgl: Parser[Array[Byte]] = "tgl" ~> address ^^ { dest => Array[Byte](TGL.code, dest, 0) }

    def codeParser: Parser[Array[Byte]] = inc | dec | cpy | jnz | tgl

    def compile(code: Seq[String]): Array[Byte] =
      code.foldLeft(Array[Byte]()) { case (compiled, line) => compiled ++ parseAll(codeParser, line).get }
  }
}
