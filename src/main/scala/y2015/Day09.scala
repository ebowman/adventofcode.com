package y2015

import scala.collection.mutable
import scala.util.parsing.combinator.RegexParsers

trait Day09 {
  class Solver(input: Seq[String]) {
    lazy val minMax: (Int, Int) = {
      val parser = new GraphParser(input)
      parser.places.toSeq.permutations.map { path =>
        path.sliding(2).map(edge => parser.paths(edge.head)(edge(1))).sum
      }.foldLeft((Int.MaxValue, Int.MinValue)) {
        case ((min, max), v) => (math.min(min, v), math.max(max, v))
      }
    }

    class GraphParser(input: Seq[String]) extends RegexParsers {
      val places: mutable.Set[String] = mutable.Set[String]()
      val paths: mutable.Map[String, mutable.Map[String, Int]] =
        new mutable.HashMap[String, mutable.Map[String, Int]]() {
          override def default(key: String): mutable.Map[String, Int] =
            put(key, mutable.Map[String, Int]())
            apply(key)
        }
      input.foreach(line => parseAll(edge, line))

      def edge: Parser[Unit] = place ~ ("to" ~> place) ~ ("=" ~> distance) ^^ {
        case from ~ to ~ dist =>
          places.addAll(Seq(from, to))
          paths(from).put(to, dist)
          paths(to).put(from, dist)
      }

      def place: Parser[String] = """\w+""".r

      def distance: Parser[Int] = """\d+""".r ^^ (_.toInt)
    }
  }

  def solve(input: Seq[String]): (Int, Int) = new Solver(input).minMax
}
