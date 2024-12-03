package y2024

class Day03 extends util.Day(3):

  def solvePart1(input: IndexedSeq[String]): Int =
    val pattern = """mul\((\d+),(\d+)\)""".r

    input.flatMap: line =>
      pattern.findAllMatchIn(line).map: m =>
        m.group(1).toInt * m.group(2).toInt
    .sum

  def solvePart2(input: IndexedSeq[String]): Int =
    case class Payload(total: Int = 0, enabled: Boolean = true)
    val pattern = """(mul\((\d+),(\d+)\)|do\(\)|don't\(\))""".r

    input.foldLeft(Payload()): (payload, line) =>
        pattern.findAllMatchIn(line).foldLeft(payload): (payload, `match`) =>
            `match`.matched match
              case "do()" => payload.copy(enabled = true)
              case "don't()" => payload.copy(enabled = false)
              case _ if payload.enabled =>
                payload.copy(total = payload.total + `match`.group(2).toInt * `match`.group(3).toInt)
              case _ => payload
    .total

end Day03
