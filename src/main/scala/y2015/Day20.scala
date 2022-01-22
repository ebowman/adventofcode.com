package y2015

trait Day20 {

  lazy val elves: LazyList[Elf] = LazyList.from(1).map(Elf.apply)
  lazy val houses: LazyList[House] = LazyList.from(1).map(House.apply)

  // from: https://www.reddit.com/r/adventofcode/comments/3xjpp2/day_20_solutions/cy59zd9/
  // using streams way too slow, both time & space inefficient :(
  def solve(n: Int): Int = {
    val houses = new Array[Int](n / 10)
    for i <- 1 to n / 10 do {
      for j <- i to n / 10 by i do {
        houses(j - 1) += i * 10
      }
    }
    houses.indexWhere(_ >= n) + 1
  }

  def solve2(n: Int): Int = {
    val houses = new Array[Int](11 * n)
    for i <- 1 to n / 11 do {
      for j <- i to i * 50 by i do {
        houses(j - 1) += i * 11
      }
    }
    houses.indexWhere(_ >= n) + 1
  }

  case class Elf(n: Int) {
    val presents: Long = 10 * n

    def visits(house: Int): Boolean = (house % n) == 0
  }

  case class House(n: Int) {
    lazy val presents: Long = elves.take(n).filter(_.visits(n)).map(_.presents).sum
  }
}