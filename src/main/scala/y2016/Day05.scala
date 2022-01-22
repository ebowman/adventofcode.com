package y2016

import java.security.MessageDigest
import scala.annotation.tailrec
import scala.util.Try

trait Day05 {

  def solve(doorId: String): (String, String) = {
    val md = MessageDigest.getInstance("MD5")

    def hash(str: String): String = {
      md.reset()
      md.digest(str.getBytes()).foldLeft(new StringBuilder) {
        case (sb, c) => sb.append(String.format("%02x", c)); sb
      }.toString()
    }

    val pass = Array[Char](0, 0, 0, 0, 0, 0, 0, 0)

    @tailrec def recurse(h: Int, sb: StringBuilder): (String, String) = {
      if sb.size >= 8 && pass.count(_ == 0) == 0 then (sb.toString().take(8), pass.mkString)
      else {
        hash(s"$doorId$h") match {
          case m if m.startsWith("00000") =>
            sb.append(m(5))
            if m(5) >= '0' && m(5) < '8' && pass(m(5) - '0') == 0 then pass(m(5) - '0') = m(6)
          case _ => ()
        }
        recurse(h + 1, sb)
      }
    }

    recurse(0, new StringBuilder)
  }
}