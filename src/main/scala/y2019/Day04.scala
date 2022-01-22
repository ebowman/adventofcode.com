package y2019

import scala.annotation.tailrec

trait Day04 {

  def twoAdjecent(str: String): Boolean = {
    @tailrec def recurse(s: String, prev: Int): Boolean = {
      if s.isEmpty then false
      else if s.head == prev then true
      else recurse(s.tail, s.head)
    }

    recurse(str.tail, str.head)
  }

  def adjecentPairs(str: String): Boolean = {
    @tailrec def recurse(s: String, seen: Map[Char, Int]): Map[Char, Int] = {
      if s.isEmpty then seen
      else recurse(s.tail, seen + (s.head -> (seen.getOrElse(s.head, 0) + 1)))
    }

    recurse(str, Map()).values.exists(_ == 2)
  }

  def neverDecreasing(str: String): Boolean = {
    @tailrec def recurse(s: String, prev: Int): Boolean = {
      if s.isEmpty then true
      else if prev > s.head then false
      else recurse(s.tail, s.head)
    }

    recurse(str.tail, str.head)
  }

  def search(min: Int, max: Int, seq: (String => Boolean)*): Int =
    (min to max).map(_.toString).count(i => seq.forall(_ (i)))

  def part1(min: Int, max: Int): Int = search(min, max, twoAdjecent, neverDecreasing)

  def part2(min: Int, max: Int): Int = search(min, max, adjecentPairs, neverDecreasing)
}
