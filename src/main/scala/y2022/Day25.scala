package y2022

trait Day25:
  // Convert a SNAFU digit to its decimal value
  private def snafuDigitToDecimal(c: Char): Int = c match
    case '2' => 2
    case '1' => 1
    case '0' => 0
    case '-' => -1
    case '=' => -2
    case _   => throw IllegalArgumentException(s"Invalid SNAFU digit: $c")

  // Convert a SNAFU number to decimal
  private def snafuToDecimal(snafu: String): Long =
    snafu.reverse.zipWithIndex.foldLeft(0L): (acc, digit) =>
      acc + snafuDigitToDecimal(digit._1) * math.pow(5, digit._2).toLong

  // Convert decimal to SNAFU
  private def decimalToSnafu(decimal: Long): String =
    if decimal == 0 then return "0"

    def nextDigit(n: Long): (Char, Long) =
      val remainder = n % 5
      remainder match
        case 0 => ('0', n / 5)
        case 1 => ('1', n / 5)
        case 2 => ('2', n / 5)
        case 3 => ('=', (n + 2) / 5)
        case 4 => ('-', (n + 1) / 5)

    val builder = StringBuilder()
    var remaining = decimal
    while remaining != 0 do
      val (digit, next) = nextDigit(remaining)
      builder.append(digit)
      remaining = next

    builder.reverse.toString

  def solvePart1(input: Seq[String]): String =
    val sum = input.map(snafuToDecimal).sum
    decimalToSnafu(sum)
end Day25
