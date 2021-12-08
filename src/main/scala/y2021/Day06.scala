package y2021

trait Day06 {
  case class Lanternfish(counter: Int) {
    def evolve: Seq[Lanternfish] =
      if (counter == 0) {
        Seq(Lanternfish(6), Lanternfish(8))
      } else Seq(copy(counter = counter - 1))
  }

  def load(str: String): Seq[Lanternfish] = str.split(",").map(_.toInt).map(c => Lanternfish(c))

  def step(fish: Seq[Lanternfish]): Seq[Lanternfish] = fish.flatMap(_.evolve)

  def recurse(fish: Seq[Lanternfish], n: Int): Seq[Lanternfish] = {
    if (n == 0) fish
    else recurse(step(fish), n - 1)
  }

  def solve2(fish: Seq[Lanternfish], n: Int): Long = {
    val blackboard = new Array[Long](9)
    fish.foreach(f => blackboard(f.counter) += 1)
    for (i <- 0 until n) {
      val cursor = i % blackboard.length
      blackboard((cursor + 7) % blackboard.length) += blackboard(cursor)
    }
    blackboard.sum
  }




}