
package y2023

import scala.annotation.tailrec
import scala.util.parsing.combinator.JavaTokenParsers

trait Parser extends JavaTokenParsers {
  override def skipWhitespace = false

  def game: Parser[Game] =
    (("Game " ~> wholeNumber) <~ ":") ~ rep1sep(drawSeq, ";") ^^ { case id ~ configs => Game(id.toInt, configs) }

  def drawSeq: Parser[Config] = repsep(draw, ",") ^^ { (data: List[(String, Int)]) =>
    @tailrec def recurse(config: Config, next: List[(String, Int)]): Config = {
      if next.isEmpty then config
      else
        val (color, n) = next.head
        val newConfig = color match {
          case "red" => config.copy(r = config.r + n)
          case "green" => config.copy(g = config.g + n)
          case "blue" => config.copy(b = config.b + n)
        }
        recurse(newConfig, next.tail)
    }

    recurse(Config(), data)
  }

  def draw: Parser[(String, Int)] = (" " ~> wholeNumber) ~ (" " ~> color) ^^ { case n ~ c => (c, n.toInt) }

  def color: Parser[String] = "red" | "green" | "blue"

  case class Config(r: Int = 0, g: Int = 0, b: Int = 0) {
    def power: Int = r * g * b
  }

  case class Game(id: Int, configs: Seq[Config])
}

// see https://adventofcode.com/2023/day/2
trait Day02 extends Parser {
  private val fullConfig = Config(12, 13, 14)

  def solve(testInput: Seq[String]): Int =
    testInput.map { input =>
      parseAll(game, input).get
    }.filter { game =>
      game.configs.forall(isLegal)
    }.map { game =>
      game.id
    }.sum

  def isLegal(config: Config): Boolean =
    config.r <= fullConfig.r && config.b <= fullConfig.b && config.g <= fullConfig.g

  def solve2(testInput: Seq[String]): Int =
    testInput.map { input =>
      parseAll(game, input).get
    }.map { game =>
      game.configs.foldLeft(Config()) { (acc, config) =>
        Config(math.max(acc.r, config.r), math.max(acc.g, config.g), math.max(acc.b, config.b))
      }.power
    }.sum
}
