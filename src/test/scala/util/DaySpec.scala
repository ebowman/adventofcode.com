package util

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.*

abstract class DaySpec(val solution: Day) extends AnyFlatSpec with Matchers:
  def load(resource: String): IndexedSeq[String] = Loader(this, resource).toIndexedSeq
  lazy val input: IndexedSeq[String] = load(s"day${solution.dayNumber}.txt")
  lazy val testInput: IndexedSeq[String] = load(s"day${solution.dayNumber}.test.txt")
  lazy val testInput2: IndexedSeq[String] = load(s"day${solution.dayNumber}.test2.txt")
