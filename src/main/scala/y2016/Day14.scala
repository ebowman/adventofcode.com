package y2016

import java.security.MessageDigest
import scala.annotation.tailrec
import scala.util.matching.Regex
import at.favre.lib.bytes.Bytes

trait Day14 {
  def salt: String

  lazy val seq: LazyList[Int] = 0 #:: 1 #:: seq.zip(seq.tail).map(n => n._2 + 1)
  lazy val saltSeq: LazyList[(Int, String)] = seq.map(i => (i, s"$salt$i"))
  lazy val hashSeq: LazyList[(Int, String)] = saltSeq.map(x => (x._1, hash(x._2)))
  lazy val stretchSeq: LazyList[(Int, String)] = hashSeq.map(x => (x._1, stretch(x._2)))
  lazy val messageDigest: MessageDigest = MessageDigest.getInstance("MD5")
  val Three: Regex = """.*?(\w)\1{2}.*""".r

  def hash(str: String): String = toHex(messageDigest.digest(str.getBytes))

  @tailrec final def stretch(str: String, n: Int = 2016): String = if n == 0 then str else stretch(hash(str), n - 1)

  def toHex(buf: Array[Byte]): String = Bytes.from(buf).encodeHex(false)

  def solve(part2: Boolean = false): Int = {
    @tailrec def recurse(input: LazyList[(Int, String)], keys: List[Int] = Nil): Int = {
      if keys.size == 64 then keys.head
      else input.head._2 match {
        case Three(reps) =>
          input.tail.take(1000).find(_._2.contains(s"${reps.head}" * 5)) match {
            case Some(_) => recurse(input.tail, input.head._1 :: keys)
            case None => recurse(input.tail, keys)
          }
        case _ => recurse(input.tail, keys)
      }
    }

    if part2 then recurse(stretchSeq) else recurse(hashSeq)
  }
}
