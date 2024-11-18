package y2020

trait Day15:
  def solve(input: String, limit: Int): Int =
    val seeds = input.split(",").map(_.toInt)
    val memory = new Array[Int](limit)

    var turn = 1
    seeds.init.foreach: num =>
      memory(num) = turn
      turn += 1

    var lastNumber = seeds.last
    while (turn < limit) do
      val prevTurn = memory(lastNumber)
      memory(lastNumber) = turn
      lastNumber = if (prevTurn == 0) 0 else turn - prevTurn
      turn += 1

    lastNumber
  end solve
end Day15