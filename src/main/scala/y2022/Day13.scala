package y2022

enum Packet:
  case IntPacket(value: Int)
  case ListPacket(values: List[Packet])

trait Day13:
  import Packet.*

  def parsePacket(s: String): Packet =
    def parseList(s: String): (List[Packet], String) =
      if s.startsWith("]") then (List(), s.tail)
      else if s.startsWith(",") then parseList(s.tail)
      else
        val (first, rest) = parseElement(s)
        val (remaining, finalRest) = parseList(rest)
        (first :: remaining, finalRest)

    def parseElement(s: String): (Packet, String) =
      if s.startsWith("[") then
        val (list, rest) = parseList(s.tail)
        (ListPacket(list), rest)
      else
        val numberStr = s.takeWhile(c => c.isDigit)
        val number = IntPacket(numberStr.toInt)
        (number, s.drop(numberStr.length))

    parseElement(s)._1

  def compare(left: Packet, right: Packet): Int = (left, right) match
    case (IntPacket(l), IntPacket(r)) => l.compare(r)
    case (ListPacket(l), ListPacket(r)) =>
      l.zip(r).map((a, b) => compare(a, b)).find(_ != 0).getOrElse(l.length.compare(r.length))
    case (IntPacket(l), r@ListPacket(_)) => compare(ListPacket(List(IntPacket(l))), r)
    case (l@ListPacket(_), IntPacket(r)) => compare(l, ListPacket(List(IntPacket(r))))

  def solvePart1(input: Seq[String]): Int =
    input
      .filterNot(_.isEmpty)
      .map(parsePacket)
      .grouped(2)
      .zipWithIndex
      .filter((pair, _) => compare(pair.head, pair.last) < 0)
      .map((_, index) => index + 1)
      .sum

  def solvePart2(input: Seq[String]): Int =
    val dividers = List("[[2]]", "[[6]]").map(parsePacket)
    val allPackets = input.filterNot(_.isEmpty).map(parsePacket) ++ dividers
    val sorted = allPackets.sortWith(compare(_, _) < 0)
    dividers.map(d => sorted.indexOf(d) + 1).product

end Day13
