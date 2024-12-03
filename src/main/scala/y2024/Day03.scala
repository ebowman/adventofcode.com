package y2024

class Day03 extends util.Day(3):

  def solvePart1(input: IndexedSeq[String]): Int =
    val pattern = """mul\((\d+),(\d+)\)""".r

    input.flatMap: line =>
      pattern.findAllMatchIn(line).map: m =>
        m.group(1).toInt * m.group(2).toInt
    .sum

  def solvePart2(input: IndexedSeq[String]): Int =
    val pattern = """(mul\((\d+),(\d+)\)|do\(\)|don't\(\))""".r

    input.foldLeft((0, true)): 
      case ((total, enabled), line) =>
        pattern.findAllMatchIn(line).foldLeft((total, enabled)):
          case ((accTotal, accEnabled), m) =>
            m.matched match
              case "do()" => (accTotal, true)
              case "don't()" => (accTotal, false)
              case mul if mul.startsWith("mul") && accEnabled =>
                (accTotal + m.group(2).toInt * m.group(3).toInt, accEnabled)
              case _ => (accTotal, accEnabled)
    ._1

end Day03
