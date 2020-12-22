package y2020

import scala.collection.mutable


trait Day22 {

  def score(x: (Int, Int)): Int = x._1 * (x._2 + 1)

  def part1(input: IndexedSeq[String]): Int = {
    case class Player(queue: mutable.Queue[Int] = mutable.Queue.empty)

    def parse(input: Iterable[String]): (Player, Player) = {
      val Num = """(\d+)""".r
      val players = input.foldLeft(List.empty[Player]) {
        case (player, line) if line.startsWith("Player") => Player() :: player
        case (player, Num(num)) => player.head.queue.enqueue(num.toInt); player
        case (player, _) => player
      }
      (players.tail.head, players.head)
    }

    val (p1, p2) = parse(input)
    while (p1.queue.nonEmpty && p2.queue.nonEmpty) {
      val play1 = p1.queue.dequeue()
      val play2 = p2.queue.dequeue()
      if (play1 > play2) {
        p1.queue.enqueue(play1)
        p1.queue.enqueue(play2)
      } else {
        p2.queue.enqueue(play2)
        p2.queue.enqueue(play1)
      }
    }

    if (p1.queue.isEmpty) {
      p2.queue.toIndexedSeq.reverse.zipWithIndex.map(score).sum
    } else {
      p1.queue.toIndexedSeq.reverse.zipWithIndex.map(score).sum
    }
  }

  def part2(input: IndexedSeq[String]): Int = {
    val Num = """(\d+)""".r
    val players = input.foldLeft(Nil: List[List[Int]]) {
      case (player, line) if line.startsWith("Player") => Nil :: player
      case (player, Num(num)) => (num.toInt :: player.head) :: player.tail
      case (player, _) => player
    }

    val player2 = mutable.Queue() ++ players.head.reverse
    val player1 = mutable.Queue() ++ players.tail.head.reverse

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
            if (deck1.size >= card1 && deck2.size >= card2)
              recursiveCombat(deck1.take(card1), deck2.take(card2))._1
            else if (card1 > card2) 1 else 2
          if (winner == 1) {
            deck1.enqueue(card1)
            deck1.enqueue(card2)
          } else {
            deck2.enqueue(card2)
            deck2.enqueue(card1)
          }
        }
      }
      (if (deck1.nonEmpty) 1 else 2, deck1, deck2)
    }

    (recursiveCombat(player1, player2) match {
      case (1, d1, _) => d1
      case (2, _, d2) => d2
    }).reverse.zipWithIndex.map(score).sum
  }
}
