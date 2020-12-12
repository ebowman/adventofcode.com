package y2015

trait Day17 {

  def solve(containers: Seq[Int], volume: Int): Int = {
    val dups = containers.diff(containers.distinct).toSet
    (for {
      n <- 1 to containers.size
      set <- containers.combinations(n) if set.sum == volume
    } yield {
      // how mahy dups appear just once? This might only work
      // when there are only a max of 2 dups in the input set
      1 << dups.count(d => set.count(_ == d) == 1)
    }).sum
  }

  def solveMin(containers: Seq[Int], volume: Int): Int = {
    val dups = containers.diff(containers.distinct).toSet
    val tmp = for {
      n <- 1 to containers.size
      set <- containers.combinations(n) if set.sum == volume
    } yield {
      // how mahy dups appear just once? This might only work
      // when there are only a max of 2 dups in the input set
      (set, 1 << dups.count(d => set.count(_ == d) == 1))
    }
    val min = tmp.minBy(_._1.size)._1.size
    tmp.filter(_._1.size == min).map(_._2).sum
  }
}