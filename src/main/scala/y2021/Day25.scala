package y2021

import scala.annotation.tailrec

trait Day25 {
  def solve1(input: Seq[String]): Int = {
    val (sizeY, sizeX) = (input.size, input.head.length)
    val nextGen: Map[Char, ((Int, Int)) => (Int, Int)] = Map(
      '>' -> { (pos: (Int, Int)) => if pos._1 == sizeX - 1 then (0, pos._2) else (pos._1 + 1, pos._2) },
      'v' -> { (pos: (Int, Int)) => if pos._2 == sizeY - 1 then (pos._1, 0) else (pos._1, pos._2 + 1) })
    val initBoard = (for y <- 0 until sizeY; x <- 0 until sizeX yield {
      if input(y)(x) != '.' then Some((x, y) -> input(y)(x)) else None
    }).flatten.toMap

    def availableMoves(board: Map[(Int, Int), Char], c: Char): Seq[((Int, Int), Char)] =
      board.filterNot { case (pos, char) =>
        board.contains(nextGen(char)(pos)) || board.get(pos).contains(if c == 'v' then '>' else 'v')
      }.toSeq

    def advance(board: Map[(Int, Int), Char], moves: Seq[((Int, Int), Char)]): Map[(Int, Int), Char] =
      board -- moves.map(_._1) ++ moves.map { case ((x, y), c) => nextGen(c)((x, y)) -> c }

    @tailrec def recurse(board: Map[(Int, Int), Char], n: Int = 0): Int = {
      val done = board.filterNot { case (pos, char) => board.contains(nextGen(char)(pos)) }.toSeq.isEmpty
      if done then (n + 1) / 2 + 1
      else if (n % 2) == 0 then recurse(advance(board, availableMoves(board, '>')), n + 1)
      else recurse(advance(board, availableMoves(board, 'v')), n + 1)
    }

    recurse(initBoard)
  }
}
