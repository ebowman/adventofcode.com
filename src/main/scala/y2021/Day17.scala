package y2021

import scala.annotation.tailrec

trait Day17 {

  def solve1(input: String): (Int, Int) = {
    val RE = """.*?x=(\d+)\.\.(\d+).*?y=(-?\d+)\.\.(-?\d+)""".r
    val (x1, x2, y1, y2) = input match {
      case RE(x1, x2, y1, y2) => (x1.toInt, x2.toInt, y1.toInt, y2.toInt)
    }

    def peak(velX: Int, velY: Int): Int = {
      var (vx, vy) = (velX, velY)
      var (x, y) = (0, 0)
      var maxY = 0

      @tailrec def recurse(n: Int): Int = {
        if (n == 0) Int.MinValue
        else {
          x += vx
          y += vy
          maxY = math.max(maxY, y)
          if (vx < 0) vx += 1 else if (vx > 0) vx -= 1
          vy -= 1
          if (x < x1 && vx <= 0 || x > x2 && vx >= 0 || y < y1 && vy <= 0) Int.MinValue
          else if (x1 <= x && x <= x2 && y1 <= y && y <= y2) maxY
          else recurse(n - 1)
        }
      }

      recurse(300)
    }

    var result = Int.MinValue
    var count = 0
    for (yVel <- -200 to 200; xVel <- 0 to 300) {
      peak(xVel, yVel) match {
        case Int.MinValue => ()
        case trial =>
          result = math.max(result, trial)
          count += 1
      }
    }
    (result, count)
  }
}
