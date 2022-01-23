package y2015

import scala.annotation.tailrec

trait Day08 {

  def escape(str: String): (Int, Int) = {
    val sb = new StringBuilder

    @tailrec def recurse(s: String): Int = {
      if s.isEmpty then sb.length
      else if s(0) == '\\' then
        s(1) match {
          case '\\' | '"' =>
            sb.append(s(1))
            recurse(s.drop(2))
          case 'x' =>
            sb.append(".")
            recurse(s.drop(4))
        }
      else
        sb.append(s.head)
        recurse(s.tail)
    }

    (str.length, recurse(str.tail.init))
  }

  def encode(str: String): (Int, Int) =
    val sb = new StringBuilder("\"")

    @tailrec def recurse(s: String): Int =
      if s.isEmpty then sb.length + 1
      else
        s(0) match {
          case '"' | '\\' => sb.append(s"\\${s(0)}")
          case c => sb.append(c)
        }
        recurse(s.tail)

    (str.length, recurse(str))
}
