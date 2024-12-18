package y2024

import scala.annotation.tailrec

class Day18 extends util.Day(18):

  def solvePart1(input: IndexedSeq[String]): Int =
    val coords = parseCoordinates(input)
    val (width, height) = gridDimensions(coords)

    val corruptedCount = if width == 7 && height == 7 then 12 else 1024
    val corrupted = coords.take(corruptedCount).toSet

    val start = (0, 0)
    val end = (width - 1, height - 1)
    bfsShortestPath(start, end, corrupted, width, height).getOrElse(-1)

  private def parseCoordinates(input: IndexedSeq[String]): IndexedSeq[(Int, Int)] =
    input.flatMap:
      line =>
        val Array(x, y) = line.split(",").map(_.toInt)
        Some((x, y))

  private def gridDimensions(coords: IndexedSeq[(Int, Int)]): (Int, Int) =
    ( coords.map(_._1).max + 1, coords.map(_._2).max + 1 )

  private def bfsShortestPath(start: (Int, Int),
                              end: (Int, Int),
                              corrupted: Set[(Int, Int)],
                              width: Int, height: Int): Option[Int] =

    def neighbors(x: Int, y: Int): List[(Int, Int)] =
      List((x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)).filter:
        case (nx, ny) =>
          nx >= 0 && ny >= 0 &&
            nx < width && ny < height &&
            !corrupted.contains((nx, ny))
    end neighbors

    @tailrec
    def recurse(queue: List[((Int, Int), Int)], visited: Set[(Int, Int)]): Option[Int] =
      queue match
        case Nil => None
        case (pos, dist) :: rest =>
          if pos == end then Some(dist)
          else
            val nextPositions = neighbors(pos._1, pos._2).filterNot(visited.contains)
            val nextVisited = visited ++ nextPositions
            val nextQueue = rest ++ nextPositions.map((_, dist + 1))
            recurse(nextQueue, nextVisited)
    end recurse

    if corrupted.contains(start) then None
    else recurse(List((start, 0)), Set(start))
  end bfsShortestPath

  def solvePart2(input: IndexedSeq[String]): (Int, Int) =
    val coords = parseCoordinates(input)
    val (width, height) = gridDimensions(coords)
    val start = (0, 0)
    val end = (width - 1, height - 1)

    @tailrec
    def findFirstUnreachable(remaining: List[(Int, Int)], corrupted: Set[(Int, Int)]): (Int, Int) =
      remaining match
        case Nil => (-1, -1)
        case nextCell :: rest =>
          val newCorrupted = corrupted + nextCell
          val dist = bfsShortestPath(start, end, newCorrupted, width, height)
          if dist.isEmpty then nextCell
          else findFirstUnreachable(rest, newCorrupted)
    end findFirstUnreachable

    findFirstUnreachable(coords.toList, Set.empty)

end Day18
