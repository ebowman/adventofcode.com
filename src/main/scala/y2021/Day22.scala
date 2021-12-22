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

  case class Cube(x1: Int, x2: Int, y1: Int, y2: Int, z1: Int, z2: Int) {
    def intersect(t: Cube): Option[Cube] =
      Cube(max(x1, t.x1), min(x2, t.x2), max(y1, t.y1), min(y2, t.y2), max(z1, t.z1), min(z2, t.z2)).nonEmpty

    def nonEmpty: Option[Cube] = if (x1 <= x2 && y1 <= y2 && z1 <= z2) Some(this) else None

    def volume(sign: Long): Long = (x2 - x1 + 1L) * (y2 - y1 + 1L) * (z2 - z1 + 1L) * sign
  }

  object CubeFactory {
    type tuple = (Int, Int, Int, Int, Int, Int)

    private def listToTuple[A](seq: Seq[A]): Product =
      Class.forName("scala.Tuple" + seq.size).getConstructors.apply(0).newInstance(seq: _*).asInstanceOf[Product]

    def apply(line: String): Cube =
      Cube.tupled(listToTuple("""-?\d+""".r.findAllIn(line).map(_.toInt).toSeq).asInstanceOf[tuple])
  }

  def map(): Map[Cube, Int] = Map.empty.withDefaultValue(0)

  @tailrec final def solve2(input: Seq[String], cubes: Map[Cube, Int] = map()): Long = {
    if (input.isEmpty) cubes.map { case (cube, sign) => cube.volume(sign) }.sum
    else {
      val cube = CubeFactory(input.head)
      solve2(input.tail, cubes.foldLeft(map() ++ Seq(cube -> 1).filter(_ => input.head.startsWith("on"))) {
        case (accum, (that, sign)) => cube.intersect(that).map(c => accum + (c -> (accum(c) - sign))).getOrElse(accum)
      }.foldLeft(cubes) { case (cubes, (c, sign)) => cubes + (c -> (cubes(c) + sign)) })
    }
  }
}
