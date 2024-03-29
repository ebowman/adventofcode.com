package y2020

import collection.mutable

trait Day15 {

  def solve(input: String, limit: Int): Int = {
    val memory = mutable.Map[Int, (Int, Int)]().withDefault(_ => (-1, -1))

    val seeds = input.split(",").map(_.toInt).toIndexedSeq
    for t <- 1 to seeds.length do memory(seeds(t - 1)) = (t, t)
    var lastSeen = seeds.last
    (seeds.length + 1 to limit).foreach(t => {
        memory(lastSeen) match {
          case (t0, t1) if t0 == t1 => lastSeen = 0
          case (_, _) => lastSeen = memory(lastSeen)._2 - memory(lastSeen)._1
        }
        if memory(lastSeen) == (-1, -1) then memory(lastSeen) = (t, t)
        else memory(lastSeen) = (memory(lastSeen)._2, t)
      })
    lastSeen
  }
}