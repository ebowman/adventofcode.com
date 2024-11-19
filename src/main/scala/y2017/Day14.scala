package y2017

trait Day14:
  private val hashCache = collection.mutable.Map[String, String]()

  private def knotHash(input: String): String =
    hashCache.getOrElseUpdate(input, {
      val lengths = input.map(_.toInt).toList ++ List(17, 31, 73, 47, 23)
      val nums = (0 to 255).toArray
      var pos, skip = 0

      for _ <- 0 until 64; len <- lengths do
        var i = pos
        var j = (pos + len - 1) % 256
        for _ <- 0 until len/2 do
          val tmp = nums(i)
          nums(i) = nums(j)
          nums(j) = tmp
          i = (i + 1) % 256
          j = (j - 1 + 256) % 256
        pos = (pos + len + skip) % 256
        skip += 1

      nums.grouped(16)
        .map(_.reduce(_ ^ _))
        .map(n => f"$n%02x")
        .mkString
    })

  private def toBinaryString(hex: String): Array[Boolean] =
    val result = new Array[Boolean](128)
    var idx = 0
    for ch <- hex do
      val num = Integer.parseInt(ch.toString, 16)
      result(idx) = (num & 8) != 0; idx += 1
      result(idx) = (num & 4) != 0; idx += 1
      result(idx) = (num & 2) != 0; idx += 1
      result(idx) = (num & 1) != 0; idx += 1
    result

  def solvePart1(input: Seq[String]): Int =
    val key = input.head
    var total = 0
    for row <- 0 until 128 do
      val hash = knotHash(s"$key-$row")
      val binary = toBinaryString(hash)
      total += binary.count(identity)
    total

  def solvePart2(input: Seq[String]): Int =
    val key = input.head
    val grid = Array.ofDim[Boolean](128, 128)
    for
      row <- 0 until 128
      hash = knotHash(s"$key-$row")
      binary = toBinaryString(hash)
      col <- binary.indices if binary(col)
    do grid(row)(col) = true

    var regions = 0
    val seen = Array.ofDim[Boolean](128, 128)

    def flood(row: Int, col: Int): Unit =
      if row >= 0 && row < 128 && col >= 0 && col < 128 &&
        grid(row)(col) && !seen(row)(col) then
        seen(row)(col) = true
        flood(row + 1, col)
        flood(row - 1, col)
        flood(row, col + 1)
        flood(row, col - 1)

    for
      row <- 0 until 128
      col <- 0 until 128
      if grid(row)(col) && !seen(row)(col)
    do
      regions += 1
      flood(row, col)

    regions
end Day14