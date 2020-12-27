package y2019

import scala.annotation.tailrec
import scala.util.Try

trait Day01 {

  def fuel(mass: Int) = mass/3 - 2

  @tailrec
  final def fuel2(accum: Int = 0)(mass: Int): Int = {
    val f = fuel(mass)
    if ( f <= 0) accum
    else fuel2(accum + f)(f)
  }

  def part1(input: IndexedSeq[String]): Long = {
    input.map(_.toInt).map(fuel).map(_.toLong).sum
  }

  def part2(input: IndexedSeq[String]): Long = {
    input.map(_.toInt).map(fuel2()).map(_.toLong).sum
  }
}
