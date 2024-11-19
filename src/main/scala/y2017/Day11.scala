package y2017

trait Day11:
  enum Direction:
    case N, NE, SE, S, SW, NW

  case class HexCoord(x: Int, y: Int, z: Int):
    def distance: Int = (x.abs + y.abs + z.abs) / 2

    def move(dir: Direction): HexCoord = dir match
      case Direction.N => HexCoord(x, y + 1, z - 1)
      case Direction.NE => HexCoord(x + 1, y, z - 1)
      case Direction.SE => HexCoord(x + 1, y - 1, z)
      case Direction.S => HexCoord(x, y - 1, z + 1)
      case Direction.SW => HexCoord(x - 1, y, z + 1)
      case Direction.NW => HexCoord(x - 1, y + 1, z)
  end HexCoord

  private def parseDirection(s: String): Direction = s match
    case "n"  => Direction.N
    case "ne" => Direction.NE
    case "se" => Direction.SE
    case "s"  => Direction.S
    case "sw" => Direction.SW
    case "nw" => Direction.NW
    case _    => throw IllegalArgumentException(s"Invalid direction: $s")

  private def followPath(moves: Seq[Direction]): (HexCoord, Int) =
    moves.foldLeft((HexCoord(0, 0, 0), 0)):
      case ((pos, maxDist), dir) =>
        val newPos = pos.move(dir)
        val newMaxDist = maxDist.max(newPos.distance)
        (newPos, newMaxDist)

  def solve(input: String): (HexCoord, Int) =
    val directions = input.split(",").map(parseDirection).toSeq
    followPath(directions)

  def solvePart1(input: String): Int = solve(input)._1.distance

  def solvePart2(input: String): Int = solve(input)._2
