package y2022

trait Day02:
  enum Shape:
    case Rock, Paper, Scissors
    def score: Int = this match
      case Rock => 1
      case Paper => 2
      case Scissors => 3

  enum Outcome:
    case Win, Loss, Draw
    def score: Int = this match
      case Win => 6
      case Draw => 3
      case Loss => 0

  object Shape:
    def fromChar(c: Char): Shape = c match
      case 'A' | 'X' => Rock
      case 'B' | 'Y' => Paper
      case 'C' | 'Z' => Scissors
      case _ => throw IllegalArgumentException(s"Invalid shape: $c")

  object Outcome:
    def fromChar(c: Char): Outcome = c match
      case 'X' => Loss
      case 'Y' => Draw
      case 'Z' => Win
      case _ => throw IllegalArgumentException(s"Invalid outcome: $c")

  def play(opponent: Shape, you: Shape): Outcome =
    (opponent, you) match
      case (s1, s2) if s1 == s2 => Outcome.Draw
      case (Shape.Rock, Shape.Paper) => Outcome.Win
      case (Shape.Paper, Shape.Scissors) => Outcome.Win
      case (Shape.Scissors, Shape.Rock) => Outcome.Win
      case _ => Outcome.Loss

  def chooseShape(opponent: Shape, desiredOutcome: Outcome): Shape =
    desiredOutcome match
      case Outcome.Draw => opponent
      case Outcome.Win => opponent match
        case Shape.Rock => Shape.Paper
        case Shape.Paper => Shape.Scissors
        case Shape.Scissors => Shape.Rock
      case Outcome.Loss => opponent match
        case Shape.Rock => Shape.Scissors
        case Shape.Paper => Shape.Rock
        case Shape.Scissors => Shape.Paper

  def scoreRound(opponent: Shape, you: Shape): Int =
    you.score + play(opponent, you).score



  def solvePart1(input: Seq[String]): Int =
    
    def parseLine(line: String): (Shape, Shape) =
      val Array(opp, you) = line.split(" ")
      (Shape.fromChar(opp(0)), Shape.fromChar(you(0)))
      
    input
      .map(parseLine)
      .map((opp, you) => scoreRound(opp, you))
      .sum

  def solvePart2(input: Seq[String]): Int =
    
    def parseLine(line: String): (Shape, Outcome) =
      val Array(opp, outcome) = line.split(" ")
      (Shape.fromChar(opp(0)), Outcome.fromChar(outcome(0)))
      
    input
      .map(parseLine)
      .map((opp, outcome) =>
        val yourShape = chooseShape(opp, outcome)
        scoreRound(opp, yourShape))
      .sum
end Day02
