package y2016

trait Day04 {
  private val R = """(.*?)(\d+)\[([a-z]+)]""".r

  private def isValid(code: String): Option[Int] = {
    val map = collection.mutable.Map[Char, Int]().withDefaultValue(0)
    code match {
      case R(letters, sectorId, checksum) =>
        letters.filterNot(_ == '-').foreach(c => map(c) += 1)
        val check = map.toSeq.sortWith {
          case (l, r) => if l._2 == r._2 then r._1 > l._1 else l._2 > r._2
        }.map(_._1).mkString
        if check.startsWith(checksum) then Some(sectorId.toInt) else None
    }
  }

  private def rotate(count: Int)(c: Char): Char = if c == '-' then ' ' else ((((c - 'a') + count) % 26) + 'a').toChar

  def solve1(input: Seq[String]): Int = input.flatMap(isValid).sum

  def solve2(input: Seq[String]): Option[Int] =
    input.map { case R(encrypted, sectorId, _) =>
        (encrypted.map(rotate(sectorId.toInt)).mkString.trim, sectorId.toInt)
    }.find(_._1 == "northpole object storage").map(_._2)
}