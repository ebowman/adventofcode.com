package y2020

import scala.annotation.tailrec

trait Day06 {

  def lines(input: Iterable[String]): Seq[Seq[String]] = {
    input.tail.foldLeft(Seq(Seq(input.head))) {
      case (seq, line) if line.isEmpty => seq :+ Seq.empty
      case (seq, line) => seq.init :+ (seq.last :+ line)
    }
  }

  def solveOr(input: Seq[Seq[String]]): Int = traverse(input, 0, _ | _)

  def traverse(input: Seq[Seq[String]], m: Int, op: (Int, Int) => Int): Int =
    input.map(_.foldLeft(m) { case (running, line) => op(running, parseWord(line)) }).map(countBits).sum

  def parseWord(word: String): Int = {
    @tailrec
    def recurse(w: String, bits: Int = 0): Int = {
      if (w.isEmpty) bits
      else recurse(w.tail, bits | (1 << w.head - 'a'))
    }

    recurse(word)
  }

  def countBits(word: Int): Int = {
    @tailrec
    def recurse(w: Int, c: Int = 0): Int = {
      if (w == 0) c
      else recurse(w >> 1, c + (w & 1))
    }

    recurse(word)
  }

  def solveAnd(input: Seq[Seq[String]]): Int = traverse(input, (1 << 27) - 1, _ & _)
}
