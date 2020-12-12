package y2015

import scala.annotation.tailrec

trait Day08 {

  def escaped(str: String): (Int, Int) = {
    @tailrec
    def recurse(s: String, b: StringBuilder = new StringBuilder): StringBuilder = {
      if (s.isEmpty) b
      else s(0) match {
        case '\\' => s(1) match {
          case '\\' =>
            b.append("\\")
            recurse(s.drop(2), b)
          case '"' =>
            b.append("\"")
            recurse(s.drop(2), b)
          case 'x' =>
            b.append(".")
            recurse(s.drop(4), b)
        }
        case c =>
          b.append(c)
          recurse(s.tail, b)
      }
    }

    (str.length, recurse(str.tail.init).length)
  }

  def encode(str: String): (Int, Int) = {
    def recurse(s: String, b: StringBuilder = new StringBuilder("\"")): StringBuilder = {
      if (s.isEmpty) b
      else {
        s(0) match {
          case '"' =>
            b.append("\\\"")
            recurse(s.tail, b)
          case '\\' =>
            b.append("\\\\")
            recurse(s.tail, b)
          case c =>
            b.append(c)
            recurse(s.tail, b)
        }
      }
    }

    val encoded = recurse(str).append("\"")
    (str.length, encoded.length)
  }
}
