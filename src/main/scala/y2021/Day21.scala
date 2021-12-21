package y2021

trait Day21 {

  def parse(lines: Seq[String]): IndexedSeq[Int] =
    Array(lines.head.split(": ")(1).toInt, lines.tail.head.split(": ")(1).toInt).toIndexedSeq

  case class DeterministicDie(var die: Int = 0) {
    def reset(): Unit = die = 0

    def roll(): Int = {
      val r = die + 1
      die = r % 100
      r
    }
  }

  def solve1(input: Seq[String]): Int = {
    val start = parse(input).toArray
    val scores = Array(0, 0)
    var rolls = 0
    var player = 0
    val die = DeterministicDie()
    while (scores.max < 1000) {
      val roll = Iterator.iterate(die.roll(), 3)(_ => die.roll()).sum
      rolls += 3
      start(player) = ((start(player) - 1 + roll) % 10) + 1
      scores(player) += start(player)
      player = (player + 1) % 2
    }
    scores.min * rolls
  }

  case class memoize[A, B, C, D](f: (A, B, C) => D) extends ((A, B, C) => D) {
    private val map = collection.mutable.Map[(A, B, C), D]()

    override def apply(a: A, b: B, c: C): D = map.getOrElseUpdate((a, b, c), f(a, b, c))
  }

  def solve2(input: Seq[String]): Long = {
    val diracDie: Seq[(Int, Int)] = (for (i <- 1 to 3; j <- 1 to 3; k <- 1 to 3) yield (i, j, k))
        .groupBy(x => x._1 + x._2 + x._3).view.mapValues(_.size).toSeq
    lazy val solve = memoize(iter)

    def iter(initPos: IndexedSeq[Int], initScores: IndexedSeq[Long], player: Int): IndexedSeq[Long] =
      diracDie.foldLeft(IndexedSeq(0L, 0L)) { case (wins, (roll, universes)) =>
        val pos = initPos.updated(player, (initPos(player) - 1 + roll) % 10 + 1)
        val scores = initScores.updated(player, initScores(player) + pos(player))
        if (scores(player) >= 21) IndexedSeq(wins(0) + universes * ((player + 1) % 2), wins(1) + universes * player)
        else solve(pos, scores, (player + 1) % 2).map(_ * universes).zip(wins).map(x => x._1 + x._2)
      }

    solve(parse(input), IndexedSeq(0, 0), 0).max
  }
}
