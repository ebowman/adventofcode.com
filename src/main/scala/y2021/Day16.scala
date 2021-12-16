package y2021

import scodec.bits._

trait Day16 {
  case class Parser(bits: String) {
    var versions: List[Int] = Nil
    def parse(pos: Int = 0): (Int, Long) = {
      def binToInt(in: String): Int = BitVector.fromBin(in).get.toInt(signed = false)
      def binToLong(in: String): Long = BitVector.fromBin(in).get.toLong(signed = false)
      var cursor = pos
      versions = binToInt(bits.slice(cursor, cursor + 3)) :: versions
      cursor += 3
      val typ = binToInt(bits.slice(cursor, cursor + 3))
      cursor += 3
      if (typ == 4) {
        val sb = new StringBuilder()
        var done = false
        while (!done) {
          val tmp = bits.slice(cursor, cursor + 5)
          cursor += 5
          sb.append(tmp.drop(1))
          if (tmp(0) == '0') done = true
        }
        (cursor, binToLong(sb.toString()))
      } else {
        val flag = bits(cursor)
        cursor += 1
        var values = List.empty[Long]
        if (flag == '0') {
          val toRead = binToInt(bits.slice(cursor, cursor + 15))
          cursor += 15
          val end = cursor + toRead
          while (cursor < end) {
            val p = Parser(bits)
            val (newCursor, value) = p.parse(cursor)
            versions = p.versions ++ versions
            cursor = newCursor
            values ::= value
          }
        } else {
          val count = binToInt(bits.slice(cursor, cursor + 11))
          cursor += 11
          for (_ <- 0 until count) {
            val p = Parser(bits)
            val (newCursor, value) = p.parse(cursor)
            versions = p.versions ++ versions
            cursor = newCursor
            values ::= value
          }
        }
        (cursor, typ match {
          case 0 => values.sum
          case 1 => values.product
          case 2 => values.tail.foldLeft(values.head)(math.min)
          case 3 => values.tail.foldLeft(values.head)(math.max)
          case 5 => if (values(1) > values(0)) 1 else 0
          case 6 => if (values(1) < values(0)) 1 else 0
          case 7 => if (values(1) == values(0)) 1 else 0
        })
      }
    }
  }

  def solve1(input: String): Long = {
    val p = Parser(BitVector.fromHex(input).get.toBin)
    p.parse()
    p.versions.sum
  }

  def solve2(input: String): Long =  Parser(BitVector.fromHex(input).get.toBin).parse()._2
}
