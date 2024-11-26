package y2022

trait Day06:
  def findMarker(input: String, markerSize: Int): Int =
    input
      .sliding(markerSize)
      .zipWithIndex
      .find((window, _) => window.distinct.length == markerSize)
      .map((_, index) => index + markerSize)
      .getOrElse(0)

  def solvePart1(input: Seq[String]): Int =
    findMarker(input.head, 4)

  def solvePart2(input: Seq[String]): Int =
    findMarker(input.head, 14)
end Day06
