package y2024

class Day10 extends util.Day(10):

  private case class Coord(x: Int, y: Int)

  private val Directions = List(Coord(-1, 0), Coord(1, 0), Coord(0, -1), Coord(0, 1))

  def solvePart1(input: IndexedSeq[String]): Int =
    val (area, trailheads) = parseInput(input)
    trailheads.map: start =>
      findTrails(start, area).map(_.last).distinct.size
    .sum

  def solvePart2(input: IndexedSeq[String]): Int =
    val (area, trailheads) = parseInput(input)
    trailheads.map: start =>
      findTrails(start, area).size
    .sum

  private def parseInput(input: IndexedSeq[String]): (Map[Coord, Int], Seq[Coord]) =

    val area = (for
      y <- input.indices
      x <- input.head.indices
    yield Coord(x, y) -> input(y)(x).asDigit).toMap

    val trailheads = area.collect:
      case (coord, 0) => coord
    .toSeq

    (area, trailheads)
  end parseInput

  private def findTrails(start: Coord, area: Map[Coord, Int]): List[List[Coord]] =

    def recurse(pos: Coord): List[List[Coord]] =
      val currentHeight = area(pos)
      if currentHeight == 9 then List(List(pos))
      else
        val nextHeight = currentHeight + 1
        for
          direction <- Directions
          nextCell = Coord(x = pos.x + direction.x, y = pos.y + direction.y)
          if area.get(nextCell).contains(nextHeight)
          trail <- recurse(nextCell)
        yield pos :: trail

    recurse(start)
  end findTrails

end Day10
