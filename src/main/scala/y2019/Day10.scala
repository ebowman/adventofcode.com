package y2019

import scala.annotation.tailrec

trait Day10 {
  @tailrec private final def gcd(n: (Int, Int)): Int = {
    if (n._2 == 0) n._1
    else gcd((n._2, n._1 % n._2))
  }

  private def parse(input: IndexedSeq[String]): Seq[(Int, Int)] = {
    for {
      y <- input.indices
      x <- input.head.indices if input(y).charAt(x) == '#'
    } yield (x, y)
  }

  def findBest(points: Seq[(Int, Int)]): ((Int, Int), Int) = {
    points.flatMap { pt =>
      points.withFilter(_ != pt)
        .map { other =>
          val d = (other._1 - pt._1, other._2 - pt._2)
          val g = math.abs(gcd(d))
          pt -> (other, (d._1 / g, d._2 / g))
        }
    }
      .groupBy(_._1)
      .view.mapValues(_.map(_._2).groupBy(_._2).size)
      .maxBy(_._2)
  }

  def part1(input: IndexedSeq[String]): Int = {
    val points = parse(input)
    findBest(points)._2
  }

  val angle: ((Int, Int)) => Double = {
    case (0, y) if y < 0 => math.Pi
    case (x, 0) if x < 0 => 3 * math.Pi / 2
    case (_, 0) => math.Pi / 2
    case (x, y) if y < 0 => math.Pi + math.atan(x.toDouble / y)
    case (x, y) if x < 0 && y > 0 => 2 * math.Pi + math.atan(x.toDouble / y)
    case (x, y) => math.atan(x.toDouble / y)
  }

  val theta: (((Int), (Int)), (Int, Int)) => Double = {
    case (p1, p2) => angle((p2._1 - p1._1, p2._2 - p1._2))
  }

  def magnitude(x: (Int, Int)): Double = x._1 * x._1 + x._2 * x._2

  def part2(input: IndexedSeq[String]): Seq[(Int, Int)] = {

    @tailrec def recurse(asteroids: Seq[Seq[(Int, Int)]],
                         cursor: Int = 0,
                         accum: Seq[(Int, Int)] = Seq.empty): Seq[(Int, Int)] = {
      if (asteroids.isEmpty) accum
      else if (cursor >= asteroids.length) recurse(asteroids, 0, accum)
      else {
        val next = asteroids(cursor)
        if (next.tail.nonEmpty) recurse(
          (asteroids.take(cursor) :+ next.tail) ++ asteroids.drop(cursor + 1),
          cursor + 1,
          accum :+ next.head)
        else recurse(asteroids.tail, cursor, accum :+ next.head)
      }
    }

    val points = parse(input)
    val loc = findBest(points)._1
    val angles = points.withFilter(_ != loc).map { other =>
      (other, (2 * math.Pi - theta(other, loc)) % (2 * math.Pi))
    }
    recurse(angles.groupBy(_._2).view.mapValues(_.map(_._1)).toSeq.sortBy(_._1).map {
      case (_, list) => list.sortWith {
        case (p1@(_, _), p2@(_, _)) => magnitude((p1._1 - loc._1, p1._2 - loc._2)) <
          magnitude((p2._1 - loc._1, p2._2 - loc._2))
      }
    })
  }
}
