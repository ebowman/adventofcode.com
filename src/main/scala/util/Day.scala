package util

abstract class Day(day: Int):
  final val dayNumber: String = f"$day%02d"
  def solvePart1(input: IndexedSeq[String]): Any
  def solvePart2(input: IndexedSeq[String]): Any
