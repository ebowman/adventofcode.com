package y2023

// see https://adventofcode.com/2023/day/16
trait Day16 {
  sealed trait Direction
  case object Up extends Direction
  case object Down extends Direction
  case object Left extends Direction
  case object Right extends Direction

  case class Pos(row: Int, col: Int)
  case class Beam(pos: Pos, dir: Direction)

  def solvePart1(input: Seq[String]): Int = countEnergizedTiles(input.toArray, Beam(Pos(0, 0), Right))

  def solvePart2(input: Seq[String]): Int = {
    val grid = input.toArray
    val (height, width) = (grid.length, grid(0).length)

    def edgeBeams = {
      val edges = Seq(
        (0 until width).map(col => Beam(Pos(0, col), Down)),           // Top edge
        (0 until width).map(col => Beam(Pos(height - 1, col), Up)),    // Bottom edge
        (0 until height).map(row => Beam(Pos(row, 0), Right)),         // Left edge
        (0 until height).map(row => Beam(Pos(row, width - 1), Left))   // Right edge
      ).flatten

      edges.distinct  // Remove duplicate corner positions
    }

    edgeBeams.map(beam => countEnergizedTiles(grid, beam)).max
  }

  private def countEnergizedTiles(grid: Array[String], startBeam: Beam): Int = {
    val (height, width) = (grid.length, grid(0).length)
    var visited = Set[Beam]()
    var energized = Set[Pos]()
    var beamQueue = scala.collection.mutable.Queue(startBeam)

    def isValid(pos: Pos) = pos.row >= 0 && pos.row < height && pos.col >= 0 && pos.col < width

    def nextBeams(beam: Beam): List[Beam] = {
      def move(dir: Direction) = dir match {
        case Up => Beam(Pos(beam.pos.row - 1, beam.pos.col), dir)
        case Down => Beam(Pos(beam.pos.row + 1, beam.pos.col), dir)
        case Left => Beam(Pos(beam.pos.row, beam.pos.col - 1), dir)
        case Right => Beam(Pos(beam.pos.row, beam.pos.col + 1), dir)
      }

      grid(beam.pos.row)(beam.pos.col) match {
        case '.' => List(move(beam.dir))
        case '/' => List(beam.dir match {
          case Right => move(Up)
          case Left => move(Down)
          case Up => move(Right)
          case Down => move(Left)
        })
        case '\\' => List(beam.dir match {
          case Right => move(Down)
          case Left => move(Up)
          case Up => move(Left)
          case Down => move(Right)
        })
        case '|' => beam.dir match {
          case Left | Right => List(move(Up), move(Down))
          case _ => List(move(beam.dir))
        }
        case '-' => beam.dir match {
          case Up | Down => List(move(Left), move(Right))
          case _ => List(move(beam.dir))
        }
      }
    }

    while (beamQueue.nonEmpty) {
      val beam = beamQueue.dequeue()
      if (isValid(beam.pos) && !visited(beam)) {
        visited += beam
        energized += beam.pos
        nextBeams(beam).filter(b => isValid(b.pos) && !visited(b)).foreach(beamQueue.enqueue(_))
      }
    }

    energized.size
  }
}