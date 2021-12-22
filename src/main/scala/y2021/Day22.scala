package y2021

import math.{max, min}
import scala.collection.mutable
import scala.annotation.tailrec

trait Day22 {
  def solve1(input: Seq[String]): Int = {
    val core = mutable.Map[(Int, Int, Int), Boolean]().withDefaultValue(false)
    input.foreach { line =>
      val Array(xMin, xMax, yMin, yMax, zMin, zMax) = """-?\d+""".r.findAllIn(line).toArray.map(_.toInt)
      for {x <- max(-50, xMin) to min(50, xMax)
           y <- max(-50, yMin) to min(50, yMax)
           z <- max(-50, zMin) to min(50, zMax)}
        if (line.startsWith("on")) core((x, y, z)) = true else core.remove((x, y, z))
    }
    core.count(_._2 == true)
  }

  case class Cube(xMin: Int, xMax: Int, yMin: Int, yMax: Int, zMin: Int, zMax: Int) {
    def intersect(that: Cube): Option[Cube] = {
      val c = Cube(max(xMin, that.xMin), min(xMax, that.xMax),
        max(yMin, that.yMin), min(yMax, that.yMax),
        max(zMin, that.zMin), min(zMax, that.zMax))
      if (c.nonEmpty) Some(c) else None
    }

    def nonEmpty: Boolean = xMin <= xMax && yMin <= yMax && zMin <= zMax

    def volume(sign: Long): Long = (xMax - xMin + 1L) * (yMax - yMin + 1L) * (zMax - zMin + 1L) * sign
  }

  private def listToTuple[A](seq: Seq[A]): Product =
    Class.forName("scala.Tuple" + seq.size).getConstructors.apply(0).newInstance(seq: _*).asInstanceOf[Product]

  def map(): Map[Cube, Int] = Map.empty.withDefaultValue(0)

  @tailrec final def solve2(input: Seq[String], cubes: Map[Cube, Int] = map()): Long = {
    type tuple = (Int, Int, Int, Int, Int, Int)
    if (input.isEmpty) cubes.map { case (cube, sign) => cube.volume(sign) }.sum
    else {
      val cube = Cube.tupled(listToTuple("""-?\d+""".r.findAllIn(input.head).map(_.toInt).toSeq).asInstanceOf[tuple])
      val accum = cubes.foldLeft(map() ++ Seq(cube -> 1).filter(_ => input.head.startsWith("on"))) {
        case (accum, (that, sign)) => cube.intersect(that).map(c => accum + (c -> (accum(c) - sign))).getOrElse(accum)
      }
      solve2(input.tail, accum.foldLeft(cubes) { case (cubes, (c, sign)) => cubes + (c -> (cubes(c) + sign)) })
    }
  }
}
