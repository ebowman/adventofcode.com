package y2019

import scala.annotation.tailrec
import scala.util.Random


trait Day15 extends Intcode {
  type Board = Map[(Int, Int), Int]

  val UNKNOWN: Int = -1
  val WALL: Int = 0
  val BLANK: Int = 1
  val OXYGEN: Int = 2

  def move(pos: (Int, Int), dir: Int): (Int, Int) = {
    dir match {
      case 1 => (pos._1, pos._2 - 1)
      case 2 => (pos._1, pos._2 + 1)
      case 3 => (pos._1 - 1, pos._2)
      case 4 => (pos._1 + 1, pos._2)
    }
  }

  def discoverGrid(code: String): ((Int, Int), Board) = {

    Random.setSeed(0) // some sequences don't properly discover the maze

    def rndDir(): Int = Random.nextInt(4) + 1

    val machine: Intcode.StepMachine = Intcode.compiler(code).stepMachine()

    @tailrec def recurse(pos: (Int, Int),
                         goal: (Int, Int),
                         dir: Int,
                         grid: Board): ((Int, Int), Board) = {

      if goal != (0, 0) && !containsReachableUnexplored(grid) then (goal, grid + (goal -> OXYGEN))
      else {
        machine.step(dir) match {
          case 0 =>
            recurse(pos, goal, dir = rndDir(), grid + (move(pos, dir) -> WALL))
          case 1 =>
            recurse(pos = move(pos, dir), goal, dir = rndDir(), grid + (pos -> BLANK))
          case 2 =>
            recurse(pos = move(pos, dir), goal = pos, dir = rndDir(), grid + (pos -> BLANK))
        }
      }
    }

    recurse(pos = (0, 0), goal = (0, 0), dir = rndDir(), grid = Map() + ((0, 0) -> BLANK))
  }

  def part1(code: String): Int = {
    val (goal, grid) = discoverGrid(code)
    var visited = Set[(Int, Int)]()
    var distance = 1
    var queue = goal :: Nil
    while queue.nonEmpty do {
      var newQueue = List.empty[(Int, Int)]
      for cell <- queue do {
        for
          dir <- 1 to 4
          neighbor = move(cell, dir) if grid(neighbor) != WALL
        do {
          if neighbor == (0, 0) then {
            distance += 1
            return distance
          } else if !visited.contains(neighbor) then {
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

  val dirs = Seq(1, 2, 3, 4)

  def containsReachableUnexplored(grid: Board): Boolean = {
    import math.{min,max}
    val (minX, maxX, minY, maxY) = grid.keys.foldLeft((Int.MaxValue, Int.MinValue, Int.MaxValue, Int.MinValue)) {
      case ((minX, maxX, minY, maxY), (x, y)) => (min(x, minX), max(x, maxX), min(y, minY), max(y, maxY))
    }
    (minX - 1 to maxX + 1).exists { x =>
      (minY - 1 to maxY + 1).exists { y =>
        !grid.contains((x, y)) &&
          dirs.exists(d => grid.getOrElse(move((x, y), d), UNKNOWN) == BLANK)
      }
    }
  }

  def part2(code: String): Int = {
    val (_, grid) = discoverGrid(code)

    def next(board: Board): Board = board ++ board.collect {
      case (p, BLANK) if dirs.exists(d => board(move(p, d)) == OXYGEN) => p -> OXYGEN
    }

    @tailrec def recurse(nxt: Board, cur: Board, count: Int = 0): Int = {
      if cur == nxt then count + 1
      else recurse(nxt = next(nxt), cur = nxt, count + 1)
    }

    recurse(next(grid), grid)
  }
}
