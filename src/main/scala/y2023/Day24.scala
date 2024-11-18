package y2023

import scala.util.boundary

/**
 *
 * Part 1 involves finding intersections between moving objects in 2D space (ignoring z).
 * This can be solved with standard line intersection calculations.
 *
 * Part 2 appears intractable at first - finding an integer position and velocity
 * that perfectly intersects all hailstones in 3D space. The search space seems enormous
 * given positions in the hundreds of trillions.
 *
 * The key insight that unlocks part 2 is a change of reference frame:
 * - Instead of finding where a rock hits each hailstone
 * - Consider the rock as stationary (velocity=0) and adjust all hailstone velocities relative to it
 * - If our rock has velocity (vx,vy,vz), each hailstone's relative velocity becomes (hx-vx, hy-vy, hz-vz)
 * - In this frame, all hailstones must pass through a single point (the rock's position)
 *
 * This transforms the problem into:
 * 1. Try reasonable rock velocities (small integers, based on input data)
 * 2. Adjust hailstone velocities relative to each candidate rock velocity
 * 3. Take a few random hailstones (4 is enough) and check if they intersect at exactly one point
 * 4. When they do, we've found our answer
 *
 * This brings the search space down from trillions (positions) * thousands (velocities)
 * to just thousands (velocities) * thousands (velocities) * thousands (velocities).
 */
// see https://adventofcode.com/2023/day/24
trait Day24:
  def solvePart1(input: Seq[String], min: Double, max: Double): Int =
    val hailstones = parseInput(input)
    countIntersectionsInArea(hailstones, min, max)

  def parseInput(input: Seq[String]): List[Hailstone] =
    input.map { line =>
      val parts = line.split("@").map(_.trim)
      val pos = parts(0).split(",").map(_.trim.toDouble)
      val vel = parts(1).split(",").map(_.trim.toDouble)
      Hailstone(
        Point(pos(0), pos(1), pos(2)),
        Velocity(vel(0), vel(1), vel(2))
      )
    }.toList

  def countIntersectionsInArea(hailstones: List[Hailstone],
                               minCoord: Double,
                               maxCoord: Double): Int =
    val intersections =
      for
        (h1, i) <- hailstones.zipWithIndex.iterator // take each hailstone
        h2 <- hailstones.drop(i + 1)
        intersection <- findIntersection(h1, h2)
        (x, y) = intersection
        if x >= minCoord && x <= maxCoord &&
          y >= minCoord && y <= maxCoord
      yield intersection
    intersections.size

  def findIntersection(h1: Hailstone, h2: Hailstone): Option[(Double, Double)] =
    h1.intersectXY(h2).map { case (x, y, _) => (x, y) }

  def solvePart2(input: Seq[String]): Long =
    val hailstones = parseInput(input)
    findRockTrajectory(hailstones)

  def findRockTrajectory(hailstones: List[Hailstone]): Long =
    val velocityRange = -500L to 500L

    boundary:
      while true do
        val selectedHailstones = scala.util.Random.shuffle(hailstones).take(4)
        val (h0, rest) = (selectedHailstones.head, selectedHailstones.tail)
        for
          dx <- velocityRange
          dy <- velocityRange
          h0Adjusted = h0.withVelocityDelta(dx, dy)
          intersections = rest.map(_.withVelocityDelta(dx, dy).intersectXY(h0Adjusted))
          if intersections.forall(_.isDefined)
          pts = intersections.map(_.get)
          if pts.forall(p => p._1 == pts.head._1 && p._2 == pts.head._2)
          dz <- velocityRange
          Seq(z1, z2, z3) = rest.zip(pts).take(3).map((h, pt) => h.predictZ(pt._3, dz))
          if z1 == z2 && z2 == z3
        do
          boundary.break((pts.head._1 + pts.head._2 + z1).round)
      0L

  case class Point(x: Double, y: Double, z: Double)

  case class Velocity(dx: Double, dy: Double, dz: Double)

  case class Hailstone(pos: Point, vel: Velocity):
    def withVelocityDelta(vx: Long, vy: Long): Hailstone =
      this.copy(vel = Velocity(vel.dx + vx, vel.dy + vy, vel.dz))

    def intersectXY(other: Hailstone): Option[(Double, Double, Double)] =
      val slope = if vel.dx == 0 then Double.NaN else vel.dy / vel.dx
      val otherSlope = if other.vel.dx == 0 then Double.NaN else other.vel.dy / other.vel.dx

      if slope.isNaN || otherSlope.isNaN || slope == otherSlope then None
      else
        val c = pos.y - slope * pos.x
        val otherC = other.pos.y - otherSlope * other.pos.x

        val x = (otherC - c) / (slope - otherSlope)
        val t1 = (x - pos.x) / vel.dx
        val t2 = (x - other.pos.x) / other.vel.dx

        if t1 < 0 || t2 < 0 then None
        else
          val y = slope * (x - pos.x) + pos.y
          Some((x, y, t1))

    def predictZ(time: Double, deltaVz: Long): Double =
      pos.z + time * (vel.dz + deltaVz)

