package y2024

class Day10 extends util.Day(10):

  def solvePart1(input: IndexedSeq[String]): Int =
    val (area, trailheads) = parseInput(input)
    trailheads.map: start =>
      findTrails(start, area).map(_.last).toSet.size
    .sum

  def solvePart2(input: IndexedSeq[String]): Int =
    val (area, trailheads) = parseInput(input)
    trailheads.map: start =>
      findTrails(start, area).size
    .sum

  private def parseInput(input: IndexedSeq[String]): (Map[(Int, Int), Int], Seq[(Int, Int)]) =
    val area = (for
      y <- input.indices
      x <- input.head.indices
    yield (x, y) -> input(y)(x).asDigit).toMap
    (area, area.keys.filter(xy => area(xy) == 0).toSeq)

  private def findTrails(start: (Int, Int), area: Map[(Int, Int), Int]): List[List[(Int, Int)]] =
    def recurse(xy: (Int, Int)): List[List[(Int, Int)]] =
      val currentHeight = area(xy)
      if currentHeight == 9 then List(List(xy))
      else
        val nextHeight = currentHeight + 1
        for
          direction <- List((-1, 0), (1, 0), (0, -1), (0, 1))
          nextCell = (xy._1 + direction._1, xy._2 + direction._2)
          if area.get(nextCell).contains(nextHeight)
          trail <- recurse(nextCell)
        yield xy :: trail

    recurse(start)
end Day10
