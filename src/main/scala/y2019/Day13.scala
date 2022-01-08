package y2019

import java.util.concurrent.LinkedBlockingQueue
import scala.collection.mutable

trait Day13 extends Intcode {

  def part1(code: String): Int = {
    val queue = new mutable.Queue[Long]
    val board = new mutable.HashMap[(Int, Int), Int]
    val sink = new Sink {
      override def put(value: Long): Unit = {
        queue.enqueue(value)
        if (queue.size == 3) {
          val x = queue.dequeue().toInt
          val y = queue.dequeue().toInt
          val tileId = queue.dequeue().toInt
          board((x, y)) = tileId
          queue.clear()
        }
      }
    }

    val machine = Intcode.compiler(code, 8 * 1024).compile(LBQSource(), sink)
    machine.execute()
    board.values.count(_ == 2)
  }

  def part2(code: String): Int = {
    val hackedCode = "2" + code.drop(1)

    val queue = new mutable.Queue[Long]
    val board = new mutable.HashMap[(Int, Int), Int]
    var paddle = (-1, -1)
    var score = 0
    val paddleCommands = new LinkedBlockingQueue[Int]

    val sink = new Sink {
      override def put(value: Long): Unit = {
        queue.enqueue(value)
        if (queue.size == 3) {
          val x = queue.dequeue().toInt
          val y = queue.dequeue().toInt
          if (x == -1 && y == 0) score = queue.dequeue().toInt
          else {
            val tileId = queue.dequeue().toInt
            board((x, y)) = tileId
            tileId match {
              case 3 =>
                paddle = (x, y)
              case 4 =>
                if (x < paddle._1) paddleCommands.put(-1)
                else if (x > paddle._1) paddleCommands.put(1)
              case _ =>
            }
          }
          queue.clear()
        }
      }
    }
    val source = new Source {
      override def take(): Long = {
        if (paddleCommands.isEmpty) paddleCommands.put(0)
        paddleCommands.take()
      }
    }

    val machine = Intcode.compiler(hackedCode, 8 * 1024).compile(source, sink)
    machine.execute()
    score
  }
}
