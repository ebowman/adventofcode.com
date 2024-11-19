package y2017

import scala.annotation.tailrec


trait Day19:
  enum Direction:
    case Up, Down, Left, Right

  case class Position(x: Int, y: Int):
    def move(dir: Direction): Position = dir match
      case Direction.Up => copy(y = y - 1)
      case Direction.Down => copy(y = y + 1)
      case Direction.Left => copy(x = x - 1)
      case Direction.Right => copy(x = x + 1)

  private def findStart(grid: Seq[String]): Position =
    Position(grid.head.indexOf('|'), 0)

  private def charAt(grid: Seq[String], pos: Position): Char =
    if pos.y >= 0 && pos.y < grid.length && pos.x >= 0 && pos.x < grid(pos.y).length then
      grid(pos.y)(pos.x)
    else ' '

  private def oppositeDirection(dir: Direction): Direction = dir match
    case Direction.Up => Direction.Down
    case Direction.Down => Direction.Up
    case Direction.Left => Direction.Right
    case Direction.Right => Direction.Left

  private def isAnyPathChar(c: Char): Boolean =
    c == '|' || c == '-' || c == '+' || c.isLetter

  private def nextDirection(grid: Seq[String], pos: Position, currentDir: Direction): Option[Direction] =
    charAt(grid, pos) match
      case ' ' => None
      case '+' =>
        val opposite = oppositeDirection(currentDir)
        Direction.values.find: dir =>
          dir != currentDir &&
            dir != opposite &&
            isAnyPathChar(charAt(grid, pos.move(dir)))
      case c =>
        val nextPos = pos.move(currentDir)
        val nextChar = charAt(grid, nextPos)
        if isAnyPathChar(nextChar) then
          Some(currentDir)
        else if c == '+' then
          val opposite = oppositeDirection(currentDir)
          Direction.values.find: dir =>
            dir != currentDir &&
              dir != opposite &&
              isAnyPathChar(charAt(grid, pos.move(dir)))
        else None

  private def navigate(grid: Seq[String]): (List[Char], Int) =
    @tailrec
    def step(pos: Position, dir: Direction, letters: List[Char], steps: Int): (List[Char], Int) =
      val current = charAt(grid, pos)
      val newLetters = if current.isLetter then current :: letters else letters
      nextDirection(grid, pos, dir) match
        case None => (newLetters, steps)
        case Some(newDir) =>
          step(pos.move(newDir), newDir, newLetters, steps + 1)
    end step
    
    val startPos = findStart(grid)
    step(startPos, Direction.Down, List.empty, 1)

  def solvePart1(input: Seq[String]): String =
    navigate(input)._1.reverse.mkString

  def solvePart2(input: Seq[String]): Int =
    navigate(input)._2
end Day19