package y2019

import scala.util.matching.Regex

trait Geom {

  case class Point(x: Int, y: Int) {
    override def equals(obj: Any): Boolean = {
      obj match { case Point(px, py) => x == px && y == py }
    }

    def manhattanDistance: Int = manhattanDistance(Point(0, 0))

    def manhattanDistance(p: Point): Int = math.abs(x - p.x) + math.abs(y - p.y)
  }

  trait Line {
    val a: Point
    val b: Point

    def length: Int

    def intersects(that: Line): Option[Point] = {
      (this, that) match {
        case (vl@VertLine(_, _), hl@HorizLine(_, _)) => intersects(hl, vl)
        case (hl@HorizLine(_, _), vl@VertLine(_, _)) => intersects(hl, vl)
        case _ => None
      }
    }

    def intersects(hl: HorizLine, vl: VertLine): Option[Point] = {
      if vl.a.x < hl.a.x then None // vl left of hl
      else if vl.a.x > hl.b.x then None // vl right of hl
      else if vl.a.y > hl.a.y then None // vl above hl
      else if vl.b.y < hl.a.y then None // vl below hl
      else Some(Point(vl.a.x, hl.a.y))
    }

    def contains(p: Point): Boolean
  }

  case class VertLine(a: Point, b: Point) extends Line {
    require(a.x == b.x)
    require(a.y < b.y)

    def contains(p: Point): Boolean = a.x == p.x && p.y >= a.y && p.y <= b.y

    override def length: Int = b.y - a.y
  }

  case class HorizLine(a: Point, b: Point) extends Line {
    require(a.y == b.y)
    require(a.x < b.x)

    def contains(p: Point): Boolean = a.y == p.y && p.x >= a.x && p.x <= b.x

    override def length: Int = b.x - a.x
  }

  object Line {
    def apply(a: Point, b: Point): Line = {
      if a.x == b.x then if a.y < b.y then VertLine(a, b) else VertLine(b, a)
      else if a.x < b.x then HorizLine(a, b) else HorizLine(b, a)
    }
  }

}

trait Day03 extends Geom {

  val R: Regex = """R(\d+)""".r
  val L: Regex = """L(\d+)""".r
  val U: Regex = """U(\d+)""".r
  val D: Regex = """D(\d+)""".r

  def mkPaths(input: Seq[String]): Seq[Seq[Line]] = {
    for line <- input yield {
      val points = line.split(",").foldLeft(Seq(Point(0, 0))) {
        case (seq, ins) => ins match {
          case R(n) => seq :+ seq.last.copy(x = seq.last.x + n.toInt)
          case L(n) => seq :+ seq.last.copy(x = seq.last.x - n.toInt)
          case U(n) => seq :+ seq.last.copy(y = seq.last.y + n.toInt)
          case D(n) => seq :+ seq.last.copy(y = seq.last.y - n.toInt)
        }
      }
      points.zip(points.tail).map { case (p1, p2) => Line(p1, p2) }
    }
  }

  def mkIntersections(paths: Seq[Seq[Line]]): Seq[Point] =
    for a <- paths.head
         b <- paths.tail.head
         pt <- a.intersects(b) if pt != Point(0, 0) yield pt


  def part1(input: IndexedSeq[String]): Int =
    mkIntersections(mkPaths(input)).minBy(_.manhattanDistance).manhattanDistance

  def part2(input: IndexedSeq[String]): Int = {
    val paths = mkPaths(input)
    val intersections = mkIntersections(paths)

    val traversals = for
      intersection <- intersections
      path <- paths
      subPathTmp = path.zip(path.tail).takeWhile(ab => !ab._1.contains(intersection)) if subPathTmp.nonEmpty
      subPath = subPathTmp.head._1 +: subPathTmp.map(_._2)
    yield (intersection, subPath.init.zip(subPath.tail).foldLeft(subPath.head.length) {
      case (d, (prev, next)) if next.contains(intersection) && (prev.a == next.a || prev.b == next.a) =>
        d + next.a.manhattanDistance(intersection)
      case (d, (prev, next)) if next.contains(intersection) && (prev.a == next.b || prev.b == next.b) =>
        d + next.b.manhattanDistance(intersection)
      case (d, (_, next)) => d + next.length
    })

    traversals.groupBy(_._1).values.map(_.map(_._2).sum).min
  }
}
