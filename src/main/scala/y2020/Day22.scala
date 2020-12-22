package y2020

import scala.collection.mutable
import scala.util.matching.Regex


trait Day22 {

  val Num: Regex = """(\d+)""".r

  def score(x: (Int, Int)): Int = x._1 * (x._2 + 1)

  def parse(input: IndexedSeq[String]): (mutable.Queue[Int], mutable.Queue[Int]) = {
    val players = input.foldLeft(Nil: List[mutable.Queue[Int]]) {
      case (player, line) if line.startsWith("Player") => new mutable.Queue[Int] :: player
      case (player, Num(num)) => player.head.enqueue(num.toInt); player
      case (player, _) => player
    }

    (players.tail.head, players.head)
  }

  def part1(input: IndexedSeq[String]): Int = {
    val (p1, p2) = parse(input)
    while (p1.nonEmpty && p2.nonEmpty) {
      val (play1, play2) = (p1.dequeue(), p2.dequeue())
      if (play1 > play2) p1.enqueue(play1, play2)
      else p2.enqueue(play2, play1)
    }
    (if (p1.isEmpty) p2 else p1).toIndexedSeq.reverse.zipWithIndex.map(score).sum
  }

  def part2(input: IndexedSeq[String]): Int = {

    def recursiveCombat(deck1: mutable.Queue[Int],
                        deck2: mutable.Queue[Int]): (Int, Seq[Int], Seq[Int]) = {
      var seen = Set[(Int, Int)]()
      while (deck1.nonEmpty && deck2.nonEmpty) {
        val hash = (deck1.hashCode(), deck2.hashCode())
        if (seen.contains(hash)) return (1, deck1, deck2)
        else {
          seen = seen + hash
          val (card1, card2) = (deck1.dequeue(), deck2.dequeue())
          val winner =
            if (deck1.size >= card1 && deck2.size >= card2) recursiveCombat(deck1.take(card1), deck2.take(card2))._1
            else if (card1 > card2) 1 else 2
          if (winner == 1) deck1.enqueue(card1, card2)
          else deck2.enqueue(card2, card1)
        }
      }
      (if (deck1.nonEmpty) 1 else 2, deck1, deck2)
    }

    val (player1, player2) = parse(input)
    (recursiveCombat(player1, player2) match {
      case (1, d1, _) => d1
      case (2, _, d2) => d2
    }).reverse.zipWithIndex.map(score).sum
  }
}
