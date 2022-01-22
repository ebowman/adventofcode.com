package y2016

import scala.collection.mutable
import scala.collection.Searching._
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

trait Day22 {

  type Coord = (Int, Int)

  case class Node(used: Int = 0, avail: Int) {
    val size: Int = used + avail
  }

  def solve1(input: Seq[String]): Int =
    val nodes = input.flatMap(Parser.parseLine)
    val sorted = nodes.sortBy(_._2.avail)
    nodes.collect { case (pt, a) if a.used > 0 =>
      val point = sorted.search((pt, Node(avail = a.used)))((x, y) => x._2.avail - y._2.avail).insertionPoint
      sorted.size - point
    }.sum

  def solve2(input: Seq[String]): Int =
    val nodes = input.flatMap(Parser.parseLine).toMap
    val (maxX, maxY) = (nodes.map(_._1._1).max + 1, nodes.map(_._1._2).max + 1)
    val maxSize = nodes.map(_._2.size).max / 2
    val hole = nodes.find(_._2.used == 0).map(_._1).get
    val hall = nodes.filter(_._2.size > maxSize).minBy(_._1._1)._1
    math.abs(hole._1 - hall._1 + 1) +
      math.abs(hole._2 - hall._2) +
      math.abs(hall._1 - maxX) + hall._2 +
      5 * (maxX - 2)

  object Parser extends RegexParsers {
    val NodeRx: Regex = """/dev/grid/node-x(\d+)-y(\d+)""".r
    val AbsSizeRx: Regex = """(\d+)T""".r
    val PerSizeRx: Regex = """(\d+)%""".r

    def node: Parser[(Int, Int)] = "/dev/grid/node[^ ]+".r ^^ {
      case NodeRx(x, y) => (x.toInt, y.toInt)
    }

    def absSize: Parser[Int] =
      """\d+T""".r ^^ {
        case AbsSizeRx(size) => size.toInt
      }

    def percSize: Parser[Int] =
      """\d+%""".r ^^ {
        case PerSizeRx(size) => size.toInt
      }

    def nodeLine: Parser[((Int, Int), Node)] = (node <~ absSize) ~ absSize ~ (absSize <~ percSize) ^^ {
      case n ~ usd ~ avail => (n, Node(usd, avail))
    }

    def parseLine(line: String): Option[((Int, Int), Node)] = parseAll(nodeLine, line) match
      case Success(result, _) => Some(result)
      case _ => None
  }
}
