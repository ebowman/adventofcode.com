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

  def part2(code: String, verbose: Boolean = false): Int = {
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
          if (verbose) printScreen(board)
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

  def printScreen(screen: mutable.Map[(Int, Int), Int]): Unit = {
    val (width, height) = (screen.keys.maxBy(_._1)._1 + 1, screen.keys.maxBy(_._2)._2 + 1)

    val display = Array.ofDim[Char](height, width)
    for (y <- 0 until height; x <- 0 until width) {
      screen.get(x, y) match {
        case Some(0) | None => display(y)(x) = ' '
        case Some(1) => display(y)(x) = '+'
        case Some(2) => display(y)(x) = '#'
        case Some(3) => display(y)(x) = '_'
        case Some(4) => display(y)(x) = 'O'
      }
    }
    display.foreach(row => println(row.mkString))
  }
}
