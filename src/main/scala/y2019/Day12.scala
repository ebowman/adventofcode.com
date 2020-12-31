package y2019

import scala.annotation.tailrec
import scala.util.matching.Regex

trait Day12 {

  case class Vec3(x: Int, y: Int, z: Int) {
    def +(that: Vec3): Vec3 = this.copy(x = x + that.x, y = y + that.y, z = z + that.z)

    def mag: Int = math.abs(x) + math.abs(y) + math.abs(z)
  }

  object Body {
    val Pos: Regex = """<x=(-?\d+), y=(-?\d+), z=(-?\d+)>""".r

    def apply(str: String): Body = str match {
      case Pos(x, y, z) => Body(Vec3(x.toInt, y.toInt, z.toInt), Vec3(0, 0, 0))
    }
  }

  case class Body(pos: Vec3, vel: Vec3) {
    def computeGravity(that: Body): Vec3 = {
      Vec3(
        if (pos.x < that.pos.x) 1 else if (pos.x > that.pos.x) -1 else 0,
        if (pos.y < that.pos.y) 1 else if (pos.y > that.pos.y) -1 else 0,
        if (pos.z < that.pos.z) 1 else if (pos.z > that.pos.z) -1 else 0
      )
    }

    def applyVelocity: Body = copy(pos = pos + vel, vel)

    def e: Int = pos.mag * vel.mag
  }

  object System {
    def load(input: Seq[String]): System = System(input.map(Body.apply))
  }

  case class System(bodies: Seq[Body]) {
    def next: System = copy(bodies = bodies.combinations(2).flatMap { case Seq(body1, body2) =>
      Seq(body1 -> body1.computeGravity(body2), body2 -> body2.computeGravity(body1))
    }.toSeq.groupBy(_._1).mapValues(_.map(_._2)).map { case (body, gravities) =>
      gravities.foldLeft(body) {
        case (body, delta) => body.copy(vel = body.vel + delta)
      }
    }.map(_.applyVelocity).toSeq)

    def e: Int = bodies.map(_.e).sum

    def iterate(n: Int): System = {
      @tailrec def recurse(s: System, n: Int): System = if (n == 0) s else recurse(s.next, n - 1)

      recurse(this, n)
    }
  }

  def part2(input: IndexedSeq[String]): BigInt = {
    val system = System.load(input)
    val (origX, origY, origZ) =
      (system.bodies.map(body => (body.pos.x, body.vel.x)).toSet,
        system.bodies.map(body => (body.pos.y, body.vel.y)).toSet,
        system.bodies.map(body => (body.pos.z, body.vel.z)).toSet)

    @tailrec def recurse(s: System, n: Int = 1, nx: Int = 0, ny: Int = 0, nz: Int = 0): Seq[Int] = {
      if (nx > 0 && ny > 0 && nz > 0) Seq(nx, ny, nz)
      else {
        val nxt = s.next
        recurse(nxt, n + 1,
          if (nx == 0 && nxt.bodies.map(body => (body.pos.x, body.vel.x)).toSet == origX) n else nx,
          if (ny == 0 && nxt.bodies.map(body => (body.pos.y, body.vel.y)).toSet == origY) n else ny,
          if (nz == 0 && nxt.bodies.map(body => (body.pos.z, body.vel.z)).toSet == origZ) n else nz)
      }
    }

    def lcm(list: Seq[Int]): Long = {
      list.foldLeft(1L) { (a, b) =>
        b * a / Stream.iterate((a, b.toLong)) { case (x, y) =>
          (y, x % y)
        }.dropWhile(_._2 != 0).head._1.abs
      }
    }

    lcm(recurse(system))
  }
}
