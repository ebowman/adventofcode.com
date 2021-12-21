package y2015

trait Day24 {
  def solve(input: Seq[String], parts: Int = 3): BigInt = {
    val packages = input.map(_.toInt)
    val weight = packages.sum / parts
    (1 to packages.size).find(n => packages.combinations(n).exists(_.sum == weight)).map { min =>
      packages.combinations(min).filter(_.sum == weight).map(_.map(BigInt.apply)).map(_.product).min
    }.get
  }
}