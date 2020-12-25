package y2020

import scala.annotation.tailrec
import scala.util.Try

trait Day24 {

  val WHITE = false
  val BLACK = true

  case class Coord(x: Int = 0, y: Int = 0) {

    def adjacent(dir: String): Coord = dir match {
      case "e" => copy(x = x + 2)
      case "se" => copy(x = x + 1, y = y + 1)
      case "sw" => copy(x = x - 1, y = y + 1)
      case "w" => copy(x = x - 2)
      case "nw" => copy(x = x - 1, y = y - 1)
      case "ne" => copy(x = x + 1, y = y - 1)
    }

    def neighbors: Seq[Coord] = Seq("e", "se", "sw", "w", "nw", "ne").map(adjacent)
  }

  @tailrec final def parse(i: String, c: Coord = Coord()): Coord = {
    if (i.isEmpty) c
    else {
      i.head match {
        case dir@('e' | 'w') => parse(i.tail, c.adjacent(dir.toString))
        case d1@('n' | 's') => i.tail.head match {
          case d2@('e' | 'w') => parse(i.drop(2), c.adjacent(d1.toString + d2))
        }
      }
    }
  }

  type Board = Map[Coord, Boolean]

  def mkBoard(init: Board = Map.empty): Map[Coord, Boolean] = Map[Coord, Boolean]() ++ init

  @tailrec final def parseBoard(input: IndexedSeq[String], b: Board = mkBoard()): Board = {
    if (input.isEmpty) b
    else {
      val coord = parse(input.head)
      parseBoard(input.tail, b + (coord -> !Try(b(coord)).getOrElse(WHITE)))
    }
  }

  def part1(input: IndexedSeq[String]): Int = {
    parseBoard(input).count(_._2)
  }

  def part2(input: IndexedSeq[String]): Int = {
    @tailrec def play(board: Board, n: Int = 100): Map[Coord, Boolean] = {
      if (n == 0) board
      else {
        val toExamine = board.keysIterator.flatMap(k => k +: k.neighbors).toSet
        play(board ++ toExamine.flatMap { coord =>
          if (board.get(coord).contains(BLACK)) {
            val blacks = coord.neighbors.count(c => Try(board(c)).getOrElse(false))
            if (blacks == 0 || blacks > 2) Some(coord -> WHITE)
            else None
          } else {
            val blacks = coord.neighbors.count(c => Try(board(c)).getOrElse(false))
            if (blacks == 2) Some(coord -> BLACK)
            else None
          }
        }, n - 1)
      }
    }

    play(parseBoard(input)).count(_._2)
  }
}
