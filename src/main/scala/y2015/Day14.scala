package y2015

import scala.util.matching.Regex

trait Day14 {

  case class Reindeer(name: String, speed: Int, duration: Int, rest: Int, score: Int = 0) {
    def dist(time: Int): Int = {
      val fullCycles = time / (duration + rest)
      val remainder = time % (duration + rest)
      val moving = remainder < duration
      if (moving) {
        fullCycles * speed * duration + remainder * speed
      } else {
        (fullCycles + 1) * speed * duration
      }
    }
  }

  // Prancer can fly 25 km/s for 6 seconds, but then must rest for 143 seconds.
  val Parse: Regex = """(.*?) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.""".r

  def mkDeer(str: String): Reindeer = {
    str match {
      case Parse(name, speed, duration, rest) => Reindeer(name, speed.toInt, duration.toInt, rest.toInt)
    }
  }

  case class Race(deer: Seq[Reindeer], time: Int = 0) {
    def next: Race = {
      val bestDist = deer.map(_.dist(time + 1)).max
      val nextDeer = deer.map { d =>
        if (d.dist(time + 1) == bestDist) d.copy(score = d.score + 1)
        else d
      }
      copy(deer = nextDeer, time = time + 1)
    }

    def winner: Int = deer.map(_.score).max

    def raceUntil(t: Int): Int = {
      @scala.annotation.tailrec
      def recurse(r: Race): Race = {
        if (r.time == t) r
        else recurse(r.next)
      }

      recurse(this).winner
    }
  }

}