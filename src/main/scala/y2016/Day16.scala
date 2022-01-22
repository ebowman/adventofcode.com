package y2016

import scala.annotation.tailrec

trait Day16 {

  @tailrec final def checksum(str: String): String = {
    val sb = new StringBuilder
    for i <- 0 until str.length - 1 by 2 do if str(i) == str(i + 1) then sb.append("1") else sb.append("0")
    if (sb.length % 2) == 0 then checksum(sb.toString()) else sb.toString()
  }

  @tailrec final def fill(str: String, bufSize: Int): String =
    if str.length >= bufSize then str.take(bufSize) else fill(str + "0" + not(str.reverse), bufSize)

  def not(str: String): String = str.map { case '1' => '0'; case '0' => '1' }

  def solve1(input: String, bufSize: Int): String = checksum(fill(input, bufSize))
}
