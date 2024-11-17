package y2023

trait Day18:
  def solvePart1(input: Seq[String]): Long =
    val instructions = parseInput(input)
    val points = getPoints(instructions)

    // Using Pick's theorem: A = i + b/2 - 1
    // Where A is the area, i is the number of interior points, and b is the number of boundary points
    // We want i + b, which is the total number of points
    // Rearranging: i + b = A + b/2 + 1
    val area = shoelaceFormula(points)
    val boundary = perimeter(points)

    area + boundary / 2 + 1

  def parseInput(input: Seq[String]): List[Instruction] =
    input.map { line =>
      val parts = line.split(" ")
      Instruction(
        direction = parts(0).head,
        steps = parts(1).toInt,
        color = parts(2).stripPrefix("(#").stripSuffix(")")
      )
    }.toList

  def solvePart2(input: Seq[String]): Long =
    val instructions = parseHexInstructions(input)
    val points = getPoints(instructions)

    val area = shoelaceFormula(points)
    val boundary = perimeter(points)

    area + boundary / 2 + 1

  def parseHexInstructions(input: Seq[String]): List[Instruction] =
    input.map { line =>
      val color = line.split(" ")(2).stripPrefix("(#").stripSuffix(")")
      // Extract steps from first 5 hex digits
      val steps = Integer.parseInt(color.take(5), 16)
      // Get direction from last hex digit
      val direction = color.last match {
        case '0' => 'R'
        case '1' => 'D'
        case '2' => 'L'
        case '3' => 'U'
      }
      Instruction(direction, steps, color)
    }.toList

  def getPoints(instructions: List[Instruction]): List[Point] =
    val start = Point(0, 0)
    instructions.foldLeft(List(start)) { case (points, Instruction(dir, steps, _)) =>
      val Point(x, y) = points.head
      val newPoint = dir match {
        case 'U' => Point(x, y - steps)
        case 'D' => Point(x, y + steps)
        case 'L' => Point(x - steps, y)
        case 'R' => Point(x + steps, y)
      }
      newPoint :: points
    }.reverse

  def shoelaceFormula(points: List[Point]): Long =
    val pairs = points.zip(points.tail :+ points.head)
    val area = pairs.map { case (p1, p2) =>
      p1.x * p2.y - p2.x * p1.y
    }.sum.abs / 2
    area

  def perimeter(points: List[Point]): Long =
    val pairs = points.zip(points.tail :+ points.head)
    pairs.map { case (p1, p2) =>
      math.abs(p2.x - p1.x) + math.abs(p2.y - p1.y)
    }.sum

  case class Point(x: Long, y: Long)

  case class Instruction(direction: Char, steps: Int, color: String)