package y2015

trait Day24 {
  def solve(input: Seq[String], parts: Int = 3): BigInt = {
    val packages = input.map(_.toInt)
    val weight = packages.sum / parts
    packages.combinations((1 to packages.size).map { n =>
      packages.combinations(n).filter(_.sum == weight).minByOption(_.size).map(_.size).getOrElse(Int.MaxValue)
    }.min).filter(_.sum == weight).map(_.map(BigInt.apply)).map(_.product).min
  }
}