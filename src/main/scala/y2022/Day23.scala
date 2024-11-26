package y2022

import scala.annotation.tailrec

trait Day23:
  enum Direction:
    case N, S, W, E, NE, NW, SE, SW

  case class Point(x: Int, y: Int):
    def +(other: Point): Point = Point(x + other.x, y + other.y)

    def neighbors: Set[Point] = Direction.values.toSet.map(move)

    def move(dir: Direction): Point = dir match
      case Direction.N => Point(x, y - 1)
      case Direction.S => Point(x, y + 1)
      case Direction.W => Point(x - 1, y)
      case Direction.E => Point(x + 1, y)
      case Direction.NE => Point(x + 1, y - 1)
      case Direction.NW => Point(x - 1, y - 1)
      case Direction.SE => Point(x + 1, y + 1)
      case Direction.SW => Point(x - 1, y + 1)

  private val directionsToCheck = List(
    (Direction.N, Set(Direction.N, Direction.NE, Direction.NW)),
    (Direction.S, Set(Direction.S, Direction.SE, Direction.SW)),
    (Direction.W, Set(Direction.W, Direction.NW, Direction.SW)),
    (Direction.E, Set(Direction.E, Direction.NE, Direction.SE))
  )

  private def parseInput(input: Seq[String]): Set[Point] =
    input.zipWithIndex.flatMap: (line, y) =>
      line.zipWithIndex.collect:
        case ('#', x) => Point(x, y)
    .toSet

  private def proposeMove(elf: Point, elves: Set[Point], directions: List[(Direction, Set[Direction])]): Option[Point] =
    if !elf.neighbors.exists(elves.contains) then None
    else
      directions.find: (_, checkDirs) =>
        checkDirs.forall(dir => !elves.contains(elf.move(dir)))
      .map: (moveDir, _) =>
        elf.move(moveDir)

  private def round(elves: Set[Point], directions: List[(Direction, Set[Direction])]): (Set[Point], List[(Direction, Set[Direction])], Boolean) =
    // Collect all proposed moves
    val proposals = elves.map(elf => elf -> proposeMove(elf, elves, directions)).toMap

    // Find which proposed positions have duplicates
    val proposedPositions = proposals.values.flatten
    val duplicatePositions = proposedPositions.groupBy(identity)
      .filter(_._2.size > 1)
      .keySet

    // Move elves to new positions if valid
    val newElves = elves.map: elf =>
      proposals(elf) match
        case Some(newPos) if !duplicatePositions.contains(newPos) => newPos
        case _ => elf

    // Check if any elf moved
    val elfMoved = newElves != elves

    // Rotate directions
    val newDirections = directions.tail :+ directions.head

    (newElves, newDirections, elfMoved)
  end round

  private def getBoundingBox(elves: Set[Point]): (Int, Int, Int, Int) =
    val minX = elves.map(_.x).min
    val maxX = elves.map(_.x).max
    val minY = elves.map(_.y).min
    val maxY = elves.map(_.y).max
    (minX, maxX, minY, maxY)

  def solvePart1(input: Seq[String]): Int =
    val initialElves = parseInput(input)

    val finalElves = (1 to 10).foldLeft((initialElves, directionsToCheck)):
      case ((elves, dirs), _) =>
        val (newElves, newDirs, _) = round(elves, dirs)
        (newElves, newDirs)
    ._1

    val (minX, maxX, minY, maxY) = getBoundingBox(finalElves)
    val area = (maxX - minX + 1) * (maxY - minY + 1)
    area - finalElves.size

  def solvePart2(input: Seq[String]): Int =
    val initialElves = parseInput(input)

    @tailrec
    def simulate(elves: Set[Point], directions: List[(Direction, Set[Direction])], roundNum: Int): Int =
      val (newElves, newDirs, elfMoved) = round(elves, directions)
      if !elfMoved then roundNum
      else simulate(newElves, newDirs, roundNum + 1)

    simulate(initialElves, directionsToCheck, 1)

end Day23