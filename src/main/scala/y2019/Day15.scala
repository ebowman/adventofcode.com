package y2019

import scala.util.{Random, Try}


trait Day15 extends Intcode {

  def move(pos: (Int, Int), dir: Int): (Int, Int) = {
    dir match {
      case 1 => (pos._1, pos._2 - 1)
      case 2 => (pos._1, pos._2 + 1)
      case 3 => (pos._1 - 1, pos._2)
      case 4 => (pos._1 + 1, pos._2)
    }
  }

  val WALL: Char = 0.toChar
  val BLANK: Char = 1.toChar
  val OXYGEN: Char = 2.toChar

  def fillGrid(code: String): ((Int, Int), Map[(Int, Int), Char]) = {
    Random.setSeed(0) // some sequences don't properly discover the maze
    def rndDir(): Int = Random.nextInt(4) + 1

    var pos = (0, 0)
    var grid = Map[(Int, Int), Char]() + (pos -> BLANK)
    var dir = rndDir()
    var goal = (0, 0)

    val sink = new Sink {
      override def put(value: Long): Unit = {
        value.toInt match {
          case 0 =>
            grid = grid + (move(pos, dir) -> WALL)
          case 1 =>
            grid = grid + (pos -> BLANK)
            pos = move(pos, dir)
          case 2 =>
            grid = grid + (pos -> BLANK)
            pos = move(pos, dir)
            goal = pos
        }
        if (goal != (0, 0) && !containsReachableUnexplored(grid)) {
          throw new RuntimeException("grid complete")
        }
      }
    }

    val source = new Source {
      override def take(): Long = {
        do {
          dir = rndDir()
        } while (grid.getOrElse(move(pos, dir), -1) == WALL)
        dir
      }
    }

    // blows an exception to indicate it found the oxygen
    Try(Intcode.compiler(code).compile(source, sink).execute())
    (goal, grid)
  }

  def part1(code: String): Int = {
    val (goal, grid) = fillGrid(code)
    var visited = Set[(Int, Int)]()
    var distance = 0
    var queue = goal :: Nil
    while (queue.nonEmpty) {
      var newQueue = List.empty[(Int, Int)]
      for (cell <- queue) {
        for {
          dir <- 1 to 4
          neighbor = move(cell, dir) if grid.getOrElse(neighbor, -1) != WALL
        } {
          if (neighbor == (0, 0)) {
            distance += 1
            return distance
          } else if (!visited.contains(neighbor)) {
            newQueue = neighbor :: newQueue
            visited = visited + neighbor
          }
        }
      }
      queue = newQueue
      distance += 1
    }
    distance
  }

  def containsReachableUnexplored(grid: Map[(Int, Int), Char]): Boolean = {
    val (minX, maxX) = (grid.keys.map(_._1).min, grid.keys.map(_._1).max)
    val (minY, maxY) = (grid.keys.map(_._2).min, grid.keys.map(_._2).max)
    (minX - 1 to maxX + 1).exists(x => (minY - 1 to maxY + 1).exists {
      y =>
        !grid.contains((x, y)) &&
          (grid.getOrElse((x - 1, y), -1) == BLANK ||
            grid.getOrElse((x + 1, y), -1) == BLANK ||
            grid.getOrElse((x, y - 1), -1) == BLANK ||
            grid.getOrElse((x, y + 1), -1) == BLANK)
    })
  }

  def part2(code: String): Int = {
    var (oxygen, grid) = fillGrid(code)
    val dirs = Seq(1, 2, 3, 4)
    grid = grid + (oxygen -> OXYGEN)

    def next(g: Map[(Int, Int), Char]): Map[(Int, Int), Char] = {
      val toFill: Set[(Int, Int)] = g.toSeq.collect {
        case (p, BLANK) if dirs.exists(d => g.getOrElse(move(p, d), -1) == OXYGEN) => p
      }.toSet
      g ++ toFill.map(_ -> OXYGEN)
    }

    var n = grid
    var prev = n
    var count = 0
    do {
      prev = n
      n = next(prev)
      count += 1
    } while (n != prev)
    count - 1
  }
}
