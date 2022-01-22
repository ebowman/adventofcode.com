package y2016

import scala.util.parsing.combinator.RegexParsers

trait Day21 {

  def rotate(arr: Array[Char], k: Int): Unit =
    reverse(arr, 0, k - 1)
    reverse(arr, k, arr.length - 1)
    reverse(arr, 0, arr.length - 1)

  def reverse(arr: Array[Char], start: Int, end: Int): Unit =
    for i <- start to start + end / 2 if i < (end - i + start) do swap(arr, i, end - i + start)

  def swap(arr: Array[Char], i: Int, j: Int): Unit =
    val tmp = arr(i)
    arr(i) = arr(j)
    arr(j) = tmp

  object Parser extends RegexParsers {
    def swapPos: Parser[Array[Char] => Unit] = ("swap position" ~> "\\d+".r) ~ ("with position" ~> "\\d+".r) ^^ {
      case left ~ right => arr => swap(arr, left.toInt, right.toInt)
    }

    def swapLet: Parser[Array[Char] => Unit] = ("swap letter" ~> "\\w".r) ~ ("with letter" ~> "\\w".r) ^^ {
      case left ~ right => arr => swap(arr, arr.indexOf(left.head), arr.indexOf(right.head))
    }

    def rotateLeft: Parser[Array[Char] => Unit] = ("rotate left" ~> """\d+""".r) <~ ("steps" | "step") ^^ {
      steps => arr => rotate(arr, steps.toInt)
    }

    def rotateRight: Parser[Array[Char] => Unit] = ("rotate right" ~> """\d+""".r) <~ ("steps" | "step") ^^ {
      steps => arr => rotate(arr, arr.length - steps.toInt)
    }

    def rotateOnLetter: Parser[Array[Char] => Unit] = "rotate based on position of letter" ~> """\w""".r ^^ {
      letter =>
        arr =>
          val toRotate = (1 + arr.indexOf(letter.head) + (if arr.indexOf(letter.head) >= 4 then 1 else 0)) % arr.length
          rotate(arr, arr.length - toRotate)
    }

    def reverseP: Parser[Array[Char] => Unit] = ("reverse positions" ~> "\\d+".r) ~ ("through" ~> "\\d+".r) ^^ {
      case from ~ to => arr => reverse(arr, from.toInt, to.toInt)
    }

    def movePos: Parser[Array[Char] => Unit] = ("move position" ~> "\\d+".r) ~ ("to position" ~> "\\d+".r) ^^ {
      case from ~ to => arr =>
        val (f, t) = (from.toInt, to.toInt)
        val tmp = arr(f)
        if f < t then for i <- f + 1 to t do arr(i - 1) = arr(i)
        else
          for i <- f + 1 until arr.length do arr(i - 1) = arr(i)
          for i <- arr.length - 1 until t by -1 do arr(i) = arr(i - 1)
        arr(t) = tmp
    }

    def allParsers: Parser[Array[Char] => Unit] =
      movePos | reverseP | rotateOnLetter | rotateLeft | rotateRight | swapLet | swapPos
  }

  def solve1(input: Seq[String], letters: String): String =
    val array = letters.toCharArray
    input.foreach { line => Parser.parseAll(Parser.allParsers, line).get.apply(array) }
    array.mkString

  def solve2(input: Seq[String], letters: String): String =
    "abcdefgh".permutations.withFilter(perm => solve1(input, perm) == letters).next()
}
