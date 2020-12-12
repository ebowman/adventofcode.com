package y2015

import scala.util.parsing.combinator.RegexParsers

// Sue 3: cars: 5, pomeranians: 4, vizslas: 1
trait Day16 extends RegexParsers {

  override def skipWhitespace = true

  def sue: Parser[Sue] = (name <~ ": ") ~ rep1sep(prop, ", ") ^^ { case sue ~ props => sue.copy(props = props.toMap) }

  def name: Parser[Sue] = "Sue " ~> """\d+""".r ^^ { n => Sue(n.toInt) }

  def prop: Parser[(String, Int)] = ("""[a-z]+""".r <~ ": ") ~ """\d+""".r ^^ { case key ~ value => (key, value.toInt) }

  case class Sue(num: Int, props: Map[String, Int] = Map()) {
    def score(report: Map[String, Int]): Int = {
      report.count { item => props.contains(item._1) && props(item._1) == item._2 }
    }

    def score2(report: Map[String, Int]): Int = {
      report.count {
        case (key, reading) if (key == "cats" || key == "trees") => props.contains(key) && props(key) > reading
        case (key, reading) if (key == "pomeranians" || key == "goldfish") => props.contains(key) && props(key) < reading
        case (key, reading) => props.contains(key) && props(key) == reading
      }
    }
  }
}