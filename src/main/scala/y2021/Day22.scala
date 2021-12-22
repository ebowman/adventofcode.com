package y2021

import scala.collection.mutable
import math.{min, max}

trait Day22 {

  def solve1(input: Seq[String]): Int = {
    val core = mutable.Map[(Int, Int, Int), Boolean]().withDefaultValue(false)
    input.foreach { line =>
      val on = line.startsWith("on")
      val Array(xmin, xmax, ymin, ymax, zmin, zmax) = """-?\d+""".r.findAllIn(line).toArray.map(_.toInt)
      for {
        x <- max(-50, xmin) to min(50, xmax)
        y <- max(-50, ymin) to min(50, ymax)
        z <- max(-50, zmin) to min(50, zmax)
      } {
        if (on) core((x, y, z)) = true
        else core.remove((x, y, z))
      }
    }
    core.count(_._2 == true)
  }

  def solve2(input: Seq[String]): Long = {
    val cubes = mutable.Map[(Int, Int, Int, Int, Int, Int), Long]().withDefaultValue(0L)
    input.foreach { line =>
      val Seq(minX, maxX, minY, maxY, minZ, maxZ) = """-?\d+""".r.findAllIn(line).map(_.toInt).toSeq
      val sign = if (line.startsWith("on")) 1 else -1
      val thisCube = mutable.Map[(Int, Int, Int, Int, Int, Int), Long]().withDefaultValue(0)
      for (((minX2, maxX2, minY2, maxY2, minZ2, maxZ2), sign2) <- cubes) yield {
        val (x1, y1, z1) = (math.max(minX, minX2), math.max(minY, minY2), math.max(minZ, minZ2))
        val (x2, y2, z2) = (math.min(maxX, maxX2), math.min(maxY, maxY2), math.min(maxZ, maxZ2))
        if (x1 <= x2 && y1 <= y2 && z1 <= z2) {
          thisCube((x1, x2, y1, y2, z1, z2)) -= sign2
        }
      }
      if (sign == 1) thisCube((minX, maxX, minY, maxY, minZ, maxZ)) += sign
      thisCube.foreach { case (k, v) => cubes(k) += v }
    }
    cubes.map { case ((minX, maxX, minY, maxY, minZ, maxZ), sign) =>
      (maxX - minX + 1L) * (maxY - minY + 1L) * (maxZ - minZ + 1L) * sign
    }.sum
  }
}
