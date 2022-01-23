package y2015

import at.favre.lib.bytes.Bytes
import java.security.MessageDigest

trait Day04 {
  val md = MessageDigest.getInstance("MD5")

  @inline private def digest(str: String): String = toHexStr(md.digest(str.getBytes))

  @inline private def toHexStr(buf: Array[Byte]): String = Bytes.from(buf).encodeHex(true)

  def solve(secret: String, zeros: Int): Int =
    Iterator.from(0).map(i => (i, digest(secret + i))).dropWhile((_, d) => !d.startsWith("0" * zeros)).next()._1

}
