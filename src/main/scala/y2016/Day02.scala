package y2016

import scala.annotation.tailrec

trait Day02 {

  def solve1(input: Seq[String]): String = {
    case class Pad(val pos: Int = 5) {
      def advance(code: String): Int = {
        if code.isEmpty then pos
        else {
          code.head match {
            case 'U' =>
              Pad(pos match {
                case 1 => 1
                case 2 => 2
                case 3 => 3
                case 4 => 1
                case 5 => 2
                case 6 => 3
                case 7 => 4
                case 8 => 5
                case 9 => 6
              }).advance(code.tail)
            case 'D' =>
              Pad(pos match {
                case 1 => 4
                case 2 => 5
                case 3 => 6
                case 4 => 7
                case 5 => 8
                case 6 => 9
                case 7 => 7
                case 8 => 8
                case 9 => 9
              }).advance(code.tail)
            case 'L' =>
              Pad(pos match {
                case 1 => 1
                case 2 => 1
                case 3 => 2
                case 4 => 4
                case 5 => 4
                case 6 => 5
                case 7 => 7
                case 8 => 7
                case 9 => 8
              }).advance(code.tail)
            case 'R' =>
              Pad(pos match {
                case 1 => 2
                case 2 => 3
                case 3 => 3
                case 4 => 5
                case 5 => 6
                case 6 => 6
                case 7 => 8
                case 8 => 9
                case 9 => 9
              }).advance(code.tail)
          }
        }
      }
    }

    @tailrec def recurse(lines: Seq[String], code: List[Int] = Nil): String = {
      if lines.isEmpty then code.reverse.mkString
      else recurse(lines.tail, Pad(code.headOption.getOrElse(5)).advance(lines.head) :: code)
    }

    recurse(input)
  }

  def solve2(input: Seq[String]): String = {
    case class Pad(val pos: Char = '5') {
      def advance(code: String): Char = {
        if code.isEmpty then pos
        else {
          code.head match {
            case 'U' =>
              Pad(pos match {
                case '1' => '1'
                case '2' => '2'
                case '3' => '1'
                case '4' => '4'
                case '5' => '5'
                case '6' => '2'
                case '7' => '3'
                case '8' => '4'
                case '9' => '9'
                case 'A' => '6'
                case 'B' => '7'
                case 'C' => '8'
                case 'D' => 'B'
              }).advance(code.tail)
            case 'D' =>
              Pad(pos match {
                case '1' => '3'
                case '2' => '6'
                case '3' => '7'
                case '4' => '8'
                case '5' => '5'
                case '6' => 'A'
                case '7' => 'B'
                case '8' => 'C'
                case '9' => '9'
                case 'A' => 'A'
                case 'B' => 'D'
                case 'C' => 'C'
                case 'D' => 'D'
              }).advance(code.tail)
            case 'L' =>
              Pad(pos match {
                case '1' => '1'
                case '2' => '2'
                case '3' => '2'
                case '4' => '3'
                case '5' => '5'
                case '6' => '5'
                case '7' => '6'
                case '8' => '7'
                case '9' => '8'
                case 'A' => 'A'
                case 'B' => 'A'
                case 'C' => 'B'
                case 'D' => 'D'
              }).advance(code.tail)
            case 'R' =>
              Pad(pos match {
                case '1' => '1'
                case '2' => '3'
                case '3' => '4'
                case '4' => '4'
                case '5' => '6'
                case '6' => '7'
                case '7' => '8'
                case '8' => '9'
                case '9' => '9'
                case 'A' => 'B'
                case 'B' => 'C'
                case 'C' => 'C'
                case 'D' => 'D'
              }).advance(code.tail)
          }
        }
      }
    }

    @tailrec def recurse(lines: Seq[String], code: List[Char] = Nil): String = {
      if lines.isEmpty then code.reverse.mkString
      else recurse(lines.tail, Pad(code.headOption.getOrElse('5')).advance(lines.head) :: code)
    }

    recurse(input)
  }
}
