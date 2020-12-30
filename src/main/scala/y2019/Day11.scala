package y2019

trait Day11 extends Intcode {

  object Robot {
    val (n_, e_, s_, w_) = (0, 1, 2, 3)
    val (_b, _w) = (0, 1)
    val (_l, _r) = (0, 1)
    val turns = Seq(Seq(3, 0, 1, 2), Seq(1, 2, 3, 0))
    val step = Seq((-1, 0), (0, 1), (1, 0), (0, -1))
  }

  class Robot(code: String, initColor: Int = 0) extends Source with Sink {

    import Robot._

    val machine: Intcode = Intcode.compiler(code).compile(source = this, sink = this)
    var grid: Map[(Int, Int), Int] = Map[(Int, Int), Int]() + ((0, 0) -> initColor)
    var pos: (Int, Int) = (0, 0)
    var dir: Int = n_

    def take(): Long = {
      grid.get(pos) match {
        case Some(value) => value
        case None =>
          grid = grid + (pos -> _b)
          _b
      }
    }

    var curHandler: Int => Unit = colorHandler

    def colorHandler(color: Int) {
      grid = grid + (pos -> color)
      curHandler = turnHandler
    }

    def turnHandler(turn: Int) {
      dir = turns(turn)(dir)
      pos = (pos._1 + step(dir)._1, pos._2 + step(dir)._2)
      curHandler = colorHandler
    }

    def put(value: Long): Unit = curHandler(value.toInt)

    def run(): Int = {
      machine.execute()
      grid.size
    }

    def dumpGrid(): String = {
      val height = grid.keys.map(_._1).max - grid.keys.map(_._1).min + 1
      val width = grid.keys.map(_._2).max - grid.keys.map(_._2).min + 1
      val array = Array.ofDim[Char](height, width)
      for {y <- 0 until height
           x <- 0 until width
           key = (y, x)} {
        array(y)(x) = grid.get(key) match {
          case Some(1) => 'X'
          case Some(0) | None => ' '
        }
      }
      array.map(_.mkString).mkString("\n")
    }
  }

}
