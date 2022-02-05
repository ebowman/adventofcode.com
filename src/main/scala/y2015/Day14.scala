package y2015

import scala.annotation.tailrec
import scala.math.Integral.Implicits._
import scala.util.matching.Regex

trait Day14:
  private lazy val comet = Reindeer("Comet", 14, 10, 127)
  private lazy val dancer = Reindeer("Dancer", 16, 11, 162)
  private val Parse: Regex = """(.*?) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.""".r

  object Reindeer:
    def apply(str: String): Reindeer = str match
      case Parse(name, speed, duration, rest) => Reindeer(name, speed.toInt, duration.toInt, rest.toInt)
  end Reindeer

  case class Reindeer(name: String, speed: Int, duration: Int, rest: Int, score: Int = 0):
    def dist(time: Int): Int =
      val (fullCycles, remainder) = time /% (duration + rest)
      if remainder < duration then (fullCycles * duration + remainder) * speed
      else (fullCycles + 1) * speed * duration
  end Reindeer

  case class Race(deer: Seq[Reindeer], time: Int = 0):
    def raceUntil(t: Int): Int = Iterator.iterate(this)(_.next).dropWhile(_.time < t).next().winner

    def next: Race =
      val bestDist = deer.map(_.dist(time + 1)).max
      copy(deer = deer.map {
        case d if d.dist(time + 1) == bestDist => d.copy(score = d.score + 1)
        case d => d
      }, time = time + 1)

    def winner: Int = deer.map(_.score).max
  end Race

  def solve1(): Int = Seq(comet.dist(2503), dancer.dist(2503)).max

  def solve2(input: Iterable[String]): Int = Race(input.map(Reindeer.apply).toSeq).raceUntil(2503)
end Day14