package y2020

import scala.util.parsing.combinator.RegexParsers

trait Day04 extends RegexParsers {
  override def skipWhitespace = false

  import collection.mutable

  val fields = new mutable.HashMap[String, String]()
  var extended = false

  val Cm = "(\\d+)cm".r
  val In = "(\\d+)in".r
  val Color = """(#[0-9a-v]{6})""".r
  val Pid = """([0-9]{9})""".r

  def isYearRange(x: String, min: Int, max: Int): Boolean = x.length == 4 && x.toInt >= min && x.toInt <= max

  val rules: Map[String, String => Boolean] = Map(
    "byr" -> { (byr: String) => isYearRange(byr, 1920, 2002) },
    "iyr" -> { iyr => isYearRange(iyr, 2010, 2020) },
    "eyr" -> { eyr => isYearRange(eyr, 2020, 2030) },
    "hgt" -> {
      case Cm(cm) => cm.toInt >= 150 && cm.toInt <= 193
      case In(in) => in.toInt >= 59 && in.toInt <= 76
      case _ => false
    },
    "hcl" -> {
      case Color(_) => true
      case _ => false
    },
    "ecl" -> {
      case "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" => true
      case _ => false
    },
    "pid" -> {
      case Pid(_) => true
      case _ => false
    }
  )

  def label: Parser[String] = """[a-z]{3}""".r

  def v: Parser[String] = """[^ ]+""".r

  def keyvalue: Parser[(String, String)] = (label <~ ":") ~ v ^^ { case l ~ v => (l, v) }

  def line: Parser[Unit] = rep1sep(keyvalue, " ") ^^ {
    _.foreach(kv => fields.put(kv._1, kv._2))
  }

  val requiredFields = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

  def validate(map: Map[String, String]): Boolean = {
    val result = requiredFields.forall(map.keySet.contains)
    if (result && extended) {
      map.forall { case (k, v) => if (!rules.contains(k)) true else rules(k)(v) }
    } else result
  }
}
