package y2017

trait Day06:
  def solvePart1(input: Seq[String]): Int = solve(input)._1
  def solvePart2(input: Seq[String]): Int = solve(input)._2

  def solve(input: Seq[String]): (Int, Int) =
    val initialBanks = input.head.split("\\s+").map(_.toInt).toVector
    findCycleLength(initialBanks)

  private def findCycleLength(initialBanks: Vector[Int]): (Int, Int) =
    def redistribute(banks: Vector[Int]): Vector[Int] =
      val maxValue = banks.max
      val maxIndex = banks.indexOf(maxValue)

      // First set the chosen bank to 0
      var newBanks = banks.updated(maxIndex, 0)
      var remaining = maxValue
      var currentIndex = (maxIndex + 1) % banks.length

      // Distribute the blocks
      while remaining > 0 do
        newBanks = newBanks.updated(currentIndex, newBanks(currentIndex) + 1)
        remaining -= 1
        currentIndex = (currentIndex + 1) % banks.length

      newBanks
    end redistribute

    var seen = Map[Vector[Int], Int]()
    var current = initialBanks
    var steps = 0

    while !seen.contains(current) do
      seen = seen + (current -> steps)
      current = redistribute(current)
      steps += 1

    // Return both the total steps and the cycle length
    (steps, steps - seen(current))
end Day06