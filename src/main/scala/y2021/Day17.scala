package y2021

import scala.annotation.tailrec

trait Day17 {
  def solve1(input: String): (Int, Int) = {
    val Array(x1, x2, y1, y2) = """-?\d+""".r.findAllIn(input).toArray.map(_.toInt)

    def peak(velX: Int, velY: Int): Int = {
      var (vx, vy) = (velX, velY)
      var (x, y) = (0, 0)
      var maxY = 0

      @tailrec def recurse(n: Int): Int = {
        if n == 0 then Int.MinValue
        else {
          x += vx
          y += vy
          maxY = math.max(maxY, y)
          vx -= math.signum(vx)
          vy -= 1
          if x < x1 && vx <= 0 || x > x2 && vx >= 0 || y < y1 && vy <= 0 then Int.MinValue
          else if x1 <= x && x <= x2 && y1 <= y && y <= y2 then maxY
          else recurse(n - 1)
        }
      }

      recurse(300)
    }

    val solutions =
      for velY <- -200 to 200; velX <- 0 to 300; t = peak(velX, velY) if t != Int.MinValue yield t
    (solutions.max, solutions.size)
  }
}
