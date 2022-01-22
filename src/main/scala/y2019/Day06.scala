package y2019

import scala.annotation.tailrec

trait Day06 {
  @tailrec final def parse(input: Seq[String],
                           orbits: Map[String, String] = Map()): Map[String, String] = {
    if input.isEmpty then orbits
    else {
      val Array(left, right) = input.head.split("\\)")
      parse(input.tail, orbits = orbits + (right -> left))
    }
  }

  def part1(rules: Seq[String]): Int = {
    val orbits = parse(rules)

    @tailrec def recurse(count: Int = 0)(planet: String): Int = {
      orbits.get(planet) match {
        case None => count
        case Some(planet) => recurse(count + 1)(planet)
      }
    }

    (orbits.keySet + "COM").toSeq.map(recurse()).sum
  }

  def part2(rules: Seq[String]): Int = {
    val orbits = parse(rules)

    @tailrec def path(planet: String, accum: Seq[String] = Seq.empty): Seq[String] = {
      orbits.get(planet) match {
        case None => accum :+ "COM"
        case Some(p) => path(p, accum :+ planet)
      }
    }

    val paths = path("YOU").reverse.zipAll(path("SAN").reverse, "", "").dropWhile(ab => ab._1 == ab._2)
    paths.count(_._1 != "") + paths.count(_._2 != "") - 2
  }
}
