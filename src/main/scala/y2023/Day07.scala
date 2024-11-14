package y2023

// see https://adventofcode.com/2023/day/7
trait Day07 {
  case class Hand(cards: String, bid: Int, useJokers: Boolean) extends Ordered[Hand] {
    private val cardValues = if (useJokers)
      "AKQT98765432J".zipWithIndex.toMap.map { case (k, v) => (k, 13 - v) }
    else
      "AKQJT98765432".zipWithIndex.toMap.map { case (k, v) => (k, 13 - v) }

    def getType: Int = {
      if (useJokers) {
        val jokerCount = cards.count(_ == 'J')
        if (jokerCount == 5) return 7 // FiveOfAKind

        val groupedWithoutJokers = cards.filterNot(_ == 'J')
          .groupBy(identity)
          .view.mapValues(_.length)
          .values.toList
          .sorted(Ordering[Int].reverse)

        val strengthenedHand = (jokerCount :: groupedWithoutJokers) match {
          case j :: h :: t => (h + j) :: t
          case other => other
        }
        typeRank(strengthenedHand)
      } else {
        typeRank(cards.groupBy(identity).view.mapValues(_.length).values.toList.sorted.reverse)
      }
    }

    private def typeRank(grouped: List[Int]): Int = grouped match {
      case 5 :: Nil => 7              // FiveOfAKind
      case 4 :: 1 :: Nil => 6         // FourOfAKind
      case 3 :: 2 :: Nil => 5         // FullHouse
      case 3 :: 1 :: 1 :: Nil => 4    // ThreeOfAKind
      case 2 :: 2 :: 1 :: Nil => 3    // TwoPair
      case 2 :: 1 :: 1 :: 1 :: Nil => 2 // OnePair
      case _ => 1                     // HighCard
    }

    def compare(that: Hand): Int = {
      val typeComparison = this.getType - that.getType
      if (typeComparison != 0) typeComparison
      else {
        this.cards.zip(that.cards)
          .map { case (c1, c2) => cardValues(c1) - cardValues(c2) }
          .find(_ != 0)
          .getOrElse(0)
      }
    }
  }

  private def solve(input: Seq[String], useJokers: Boolean): Int = {
    val hands = input.map { line =>
      val parts = line.split(" ")
      Hand(parts(0), parts(1).toInt, useJokers)
    }
    hands.sorted.zipWithIndex.map { case (hand, index) =>
      hand.bid * (index + 1)
    }.sum
  }

  def solvePart1(input: Seq[String]): Int = solve(input, false)
  def solvePart2(input: Seq[String]): Int = solve(input, true)
}