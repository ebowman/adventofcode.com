package y2015

import scala.util.parsing.combinator.RegexParsers

trait Day09 extends RegexParsers {

  import scala.collection.mutable
  val places = new mutable.HashSet[String]()
  val paths = new mutable.HashMap[String, mutable.HashMap[String, Int]]()

  private def newPath(to: String, from: String, dist: Int): Unit = {
    places.add(from)
    places.add(to)
    paths.getOrElseUpdate(from, new mutable.HashMap[String, Int]()).put(to, dist)
    paths.getOrElseUpdate(to, new mutable.HashMap[String, Int]()).put(from, dist)
  }

  // Faerun to Norrath = 144
  private def place: Parser[String] = """[a-zA-Z]+""".r
  private def distance: Parser[Int] = """\d+""".r ^^ { _.toInt }
  def edge: Parser[Unit] = place ~ ("to" ~> place) ~ ("=" ~> distance) ^^ {
    case from ~ to ~ dist => newPath(from, to, dist)
  }
}
