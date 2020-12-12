package y2015

import java.security.MessageDigest

object Day04 extends App {
  def hexify(buf: Array[Byte]): String = buf.map("%02X".format(_)).mkString

  def digest(str: String): String = hexify(MessageDigest.getInstance("MD5").digest(str.getBytes))

  def solve(secret: String): Int =
    Stream.from(0).map(i => (i, digest(secret + i))).find(_._2.startsWith("00000")).map(_._1).get

  println(solve("ckczppom"))
}
