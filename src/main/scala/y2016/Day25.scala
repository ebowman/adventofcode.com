package y2016

import scala.collection.mutable
import scala.util.Try
import scala.util.parsing.combinator.RegexParsers

trait Day25 {

  import Compiler.OpCode.*

  val step = 3
  val toReg = Array("a", "b", "c", "d")

  def solve(input: Seq[String]): Int =
    val list: LazyList[(Int, Boolean)] = LazyList.from(0).map { a =>
      val machine = new Machine(Compiler.compile(input), mutable.Map("a" -> a).withDefaultValue(0))
      machine.execute(1000000)
      (a, machine.isCyclic)
    }
    list.dropWhile(_._2 == false).head._1

  class Machine(code: Array[Int], val registers: mutable.Map[String, Int]) {
    var cursor = 0
    var out: List[Int] = Nil

    def isCyclic: Boolean = out.zip(out.tail).forall((a, b) => a != b)

    def load(c: Int): Int = if code(c) >= 1000 then registers(s"${(code(c) - 1000 + 'a').toChar}") else code(c)

    def reg(c: Int): Option[String] = if code(c) >= 1000 then Some(toReg(code(c) - 1000)) else None

    def hasNext(): Boolean = cursor <= code.length - step

    def next(): Unit =
      code(cursor) match {
        case CPY.code =>
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
        case OUT.code =>
          val nextOut = load(cursor + 1)
          if out.isEmpty || out.head != nextOut then
            out ::= load(cursor + 1)
            cursor += step
          else
            out ::= load(cursor + 1)
            cursor = code.length
      }

    def execute(n: Int): Int =
      var i = 0
      while (i < n && hasNext())
        next()
        i += 1
      registers("a")
  }

  object Compiler extends RegexParsers {
    enum OpCode(val code: Int):
      case CPY extends OpCode(1)
      case INC extends OpCode(2)
      case DEC extends OpCode(3)
      case JNZ extends OpCode(4)
      case OUT extends OpCode(6)

    def addressToByte(str: String): Int = if str.head.isLetter then (str.head - 'a' + 1000) else str.toInt

    def address: Parser[Int] = """-?\w+""".r ^^ addressToByte

    def cpy: Parser[Array[Int]] = "cpy" ~> address ~ address ^^ { case src ~ dest => Array[Int](CPY.code, src, dest) }

    def inc: Parser[Array[Int]] = "inc" ~> address ^^ { dest => Array(INC.code, dest, 0) }

    def dec: Parser[Array[Int]] = "dec" ~> address ^^ { dest => Array(DEC.code, dest, 0) }

    def jnz: Parser[Array[Int]] = "jnz" ~> address ~ address ^^ { case src ~ dest => Array[Int](JNZ.code, src, dest) }

    def out: Parser[Array[Int]] = "out" ~> address ^^ { dest => Array[Int](OUT.code, dest, 0) }

    def codeParser: Parser[Array[Int]] = inc | dec | cpy | jnz | out

    def compile(code: Seq[String]): Array[Int] =
      code.foldLeft(Array[Int]()) { case (compiled, line) => compiled ++ parseAll(codeParser, line).get }
  }
}
