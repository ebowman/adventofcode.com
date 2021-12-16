package y2019

import java.util.concurrent.{ArrayBlockingQueue, Executors, TimeUnit}
import scala.annotation.tailrec

trait Day07 {

  object Intcode {
    def compile(code: String, input: ArrayBlockingQueue[Int], output: ArrayBlockingQueue[Int]): Intcode = {
      val mem = code.split(",").map(_.toInt)
      Intcode(mem, input, output)
    }
  }

  case class PackedInstruction(byte: Int) {
    lazy val str: String = byte.toString.reverse
    lazy val opcode: Int = str.take(2).reverse.toInt
    lazy val mode1: Boolean = str.slice(2, 3) == "1"
    lazy val mode2: Boolean = str.slice(3, 4) == "1"
    lazy val mode3: Boolean = str.slice(4, 5) == "1"
  }

  case class Intcode(memory: Array[Int],
                     inputs: ArrayBlockingQueue[Int],
                     outputs: ArrayBlockingQueue[Int]) {

    @tailrec final def execute(cursor: Int = 0): Unit = {
      def read(c: Int, mode: Boolean): Int = if (mode) memory(c) else memory(memory(c))

      val (iADD, iMUL, iIN, iOUT, iJIT, iJIF, iSTLT, iSTEQ) = (1, 2, 3, 4, 5, 6, 7, 8)
      PackedInstruction(memory(cursor)) match {
        case op if op.opcode == iADD =>
          val op1 = read(cursor + 1, op.mode1)
          val op2 = read(cursor + 2, op.mode2)
          memory(memory(cursor + 3)) = op1 + op2
          execute(cursor + 4)
        case op if op.opcode == iMUL =>
          val op1 = read(cursor + 1, op.mode1)
          val op2 = read(cursor + 2, op.mode2)
          memory(memory(cursor + 3)) = op1 * op2
          execute(cursor + 4)
        case op if op.opcode == iIN =>
          val input = inputs.take()
          memory(memory(cursor + 1)) = input
          execute(cursor + 2)
        case op if op.opcode == iOUT =>
          val output = memory(memory(cursor + 1))
          outputs.put(output)
          execute(cursor + 2)
        case op if op.opcode == iJIT =>
          val op1 = read(cursor + 1, op.mode1)
          val op2 = read(cursor + 2, op.mode2)
          if (op1 != 0) execute(op2)
          else execute(cursor + 3)
        /*
      case op if op.opcode == iJIF =>
        val op1 = read(cursor + 1, op.mode1)
        val op2 = read(cursor + 2, op.mode2)
        if (op1 == 0) execute(op2)
        else execute(cursor + 3)
         */
        case op if op.opcode == iSTLT =>
          val op1 = read(cursor + 1, op.mode1)
          val op2 = read(cursor + 2, op.mode2)
          if (op1 < op2) memory(memory(cursor + 3)) = 1
          else memory(memory(cursor + 3)) = 0
          execute(cursor + 4)
        /*
      case op if op.opcode == iSTEQ =>
        val op1 = read(cursor + 1, op.mode1)
        val op2 = read(cursor + 2, op.mode2)
        if (op1 == op2) memory(memory(cursor + 3)) = 1
        else memory(memory(cursor + 3)) = 0
        execute(cursor + 4)
         */
        case stop if stop.opcode == 99 =>
          ()
        case err => sys.error(s"Unknown opcode $err")
      }
    }
  }

  def executeChain(code: String, phases: Seq[Int]): Int = {
    val pipelines = phases.map { phase =>
      val input = new ArrayBlockingQueue[Int](10)
      input.put(phase)
      input
    } :+ new ArrayBlockingQueue[Int](10)

    val machines = pipelines.zip(pipelines.tail).map { case (input, output) => Intcode.compile(code, input, output) }
    machines.head.inputs.put(0)
    machines.foreach(_.execute())
    machines.last.outputs.take()
  }

  def executeFeedback(code: String, phases: Seq[Int]): Int = {

    val queues = phases.map { phase =>
      val queue = new ArrayBlockingQueue[Int](10)
      queue.put(phase)
      queue
    }

    val ring = queues :+ queues.head
    ring.head.put(0)
    val machines = ring.zip(ring.tail).map { case (input, output) => Intcode.compile(code, input, output) }
    val executor = Executors.newFixedThreadPool(10)

    machines.foreach { machine =>
      executor.submit(new Runnable {
        override def run(): Unit = {
          machine.execute()
        }
      })
    }

    executor.shutdown()
    executor.awaitTermination(Long.MaxValue, TimeUnit.DAYS)
    ring.last.take()
  }
}
