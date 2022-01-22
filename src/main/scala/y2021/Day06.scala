package y2021

import scala.annotation.tailrec

trait Day06 {
  case class LanternFish(counter: Int) {
    def evolve: Seq[LanternFish] =
      if counter == 0 then Seq(LanternFish(6), LanternFish(8))
      else Seq(copy(counter = counter - 1))
  }

  object LanternFish {
    def apply(str: String): Seq[LanternFish] = str.split(",").map(_.toInt).map(c => LanternFish(c)).toSeq
  }

  @tailrec final def solve1(fish: Seq[LanternFish], n: Int): Seq[LanternFish] =
    if n == 0 then fish
    else solve1(fish.flatMap(_.evolve), n - 1)

  def solve2(fish: Seq[LanternFish], n: Int): Long = {
    val blackboard = new Array[Long](9)
    fish.foreach(f => blackboard(f.counter) += 1)
    for i <- 0 until n do {
      val cursor = i % blackboard.length
      blackboard((cursor + 7) % blackboard.length) += blackboard(cursor)
    }
    blackboard.sum
  }
}