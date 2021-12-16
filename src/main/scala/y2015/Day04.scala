package y2015

import java.security.MessageDigest

object Day04 extends App {
  def solve(secret: String): Int =
    LazyList.from(0).map(i => (i, digest(secret + i))).find(_._2.startsWith("00000")).map(_._1).get

  def digest(str: String): String = hexify(MessageDigest.getInstance("MD5").digest(str.getBytes))

  def hexify(buf: Array[Byte]): String = buf.map("%02X".format(_)).mkString

  println(solve("ckczppom"))
}
