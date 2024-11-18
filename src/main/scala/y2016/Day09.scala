package y2016

trait Day09:
  def solve1(input: String): Int =
    var pos = 0
    var result = 0

    while (pos < input.length) do
      input(pos) match
        case '(' =>
          // Find the closing parenthesis
          val closePos = input.indexOf(')', pos)
          val Array(length, repeat) = input.substring(pos + 1, closePos).split('x').map(_.toInt)
          result += length * repeat
          pos = closePos + length + 1
        case _ =>
          result += 1
          pos += 1
    result

  def solve2(input: String): Long =
    def getDecompressedLength(str: String, start: Int, end: Int): Long =
      var pos = start
      var result = 0L

      while (pos < end) do
        str(pos) match
          case '(' =>
            val closePos = str.indexOf(')', pos)
            val Array(length, repeat) = str.substring(pos + 1, closePos).split('x').map(_.toInt)
            val segmentStart = closePos + 1
            val segmentEnd = segmentStart + length
            val decompressedSegmentLength = getDecompressedLength(str, segmentStart, segmentEnd)
            result += decompressedSegmentLength * repeat
            pos = segmentEnd
          case _ =>
            result += 1
            pos += 1
      result
    end getDecompressedLength

    getDecompressedLength(input, 0, input.length)
end Day09