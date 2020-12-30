package y2019

import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue}
import scala.annotation.tailrec

trait Intcode {

  trait Source {
    def take(): Long
  }

  trait Sink {
    def put(value: Long): Unit
  }

  case class LBQSource(queue: LinkedBlockingQueue[Long]) extends Source {
    def take(): Long = queue.take()
  }

  case class LBQSink(queue: LinkedBlockingQueue[Long] = new LinkedBlockingQueue[Long]) extends Sink {
    def put(value: Long) { queue.put(value) }
  }

  object Intcode {

    private def compileStr(code: String, bytes: Int): Array[Long] = {
      val tmp = code.split(",").map(_.toLong)
      tmp ++ new Array[Long](bytes - tmp.length)
    }

    def compiler(code: String, bytes: Int = 2048): Compiler = Compiler(code, bytes)

    case class Compiler(code: String, bytes: Int) {
      def compile(inputs: Seq[Long] = Seq.empty): Intcode = {
        val compiled = compileStr(code, bytes)
        val input = new LinkedBlockingQueue[Long]
        inputs.foreach(input.put)
        Intcode(compiled, LBQSource(input), LBQSink())
      }

      def compile(source: Source, sink: Sink): Intcode = {
        val compiled = compileStr(code, bytes)
        Intcode(compiled, source, sink)
      }
    }

  }

  case class Intcode(memory: Array[Long], source: Source, sink: Sink) {
    private var relativeBase: Int = 0

    private def readMem(c: Int, mode: Int): Long = {
      val next = memory(c)
      mode match {
        case 0 => memory(next.toInt)
        case 1 => next
        case 2 => memory((relativeBase + next).toInt)
      }
    }

    private def writeMem(value: Long, c: Int, mode: Int): Unit = {
      mode match {
        case 0 => memory(memory(c).toInt) = value
        case 2 => memory(relativeBase + memory(c).toInt) = value
      }
    }

    private object Instructions extends Enumeration {
      type OpCode = Value
      val Add, Mul, In, Out, Jit, Jif, Stlt, Steq, Arb, Stop = Value
      val byId = Map(1 -> Add, 2 -> Mul, 3 -> In, 4 -> Out, 5 -> Jit, 6 -> Jif, 7 -> Stlt, 8 -> Steq, 9 -> Arb, 99 -> Stop)
    }

    import Instructions._

    private object Instruction {
      @tailrec private def modes(x: Int, seq: Seq[Int] = Seq.empty): Seq[Int] =
        if (seq.length == 3) seq else modes(x / 10, seq :+ (x - 10 * (x / 10)))

      def apply(op: Int): Instruction = Instruction(byId(op - 100 * (op / 100)), modes(op / 100))
    }

    private final case class Instruction(opCode: OpCode, modes: Seq[Int]) {
      private var localCursor = 1

      def read()(implicit cursor: Int): Long = {
        localCursor += 1
        readMem(cursor + localCursor - 1, modes(localCursor - 2))
      }

      def write(value: Long)(implicit cursor: Int): Unit = {
        localCursor += 1
        writeMem(value, cursor + localCursor - 1, modes(localCursor - 2))
      }

      def next()(implicit cursor: Int): Int = cursor + localCursor
    }

    @tailrec final def execute(cursor: Int = 0): Unit = {
      implicit val cur: Int = cursor
      Instruction(memory(cursor).toInt) match {
        case op@Instruction(Add, _) =>
          op.write(op.read() + op.read())
          execute(op.next())
        case op@Instruction(Mul, _) =>
          op.write(op.read() * op.read())
          execute(op.next())
        case op@Instruction(In, _) =>
          op.write(source.take())
          execute(op.next())
        case op@Instruction(Out, _) =>
          sink.put(op.read())
          execute(op.next())
        case op@Instruction(Jit, _) =>
          val value = op.read()
          val jump = op.read().toInt
          if (value != 0) execute(jump)
          else execute(op.next())
        case op@Instruction(Jif, _) =>
          val value = op.read()
          val jump = op.read().toInt
          if (value == 0) execute(jump)
          else execute(op.next())
        case op@Instruction(Stlt, _) =>
          val op1 = op.read()
          val op2 = op.read()
          op.write(if (op1 < op2) 1 else 0)
          execute(op.next())
        case op@Instruction(Steq, _) =>
          val op1 = op.read()
          val op2 = op.read()
          op.write(if (op1 == op2) 1 else 0)
          execute(op.next())
        case op@Instruction(Arb, _) =>
          relativeBase += op.read().toInt
          execute(op.next())
        case Instruction(Stop, _) => ()
      }
    }
  }

}