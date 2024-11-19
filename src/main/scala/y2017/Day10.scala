package y2017

trait Day10:
  def solvePart1(input: Seq[String]): Int =
    val lengths = input.head.trim.split(",").map(_.toInt).toSeq
    val listSize = if lengths.length == 4 then 5 else 256
    val list = (0 until listSize)
    val result = processKnotHash(list, lengths)
    result(0) * result(1)
  end solvePart1

  private def processKnotHash(initialList: IndexedSeq[Int],
                              lengths: Seq[Int]): IndexedSeq[Int] =
    processRound(KnotState(initialList), lengths).list

  private def processRound(state: KnotState,
                           lengths: Seq[Int]): KnotState =
    lengths.foldLeft(state): (state, length) =>
      val newList = reverseSubList(state.list, state.currentPos, length)
      val newPos = (state.currentPos + length + state.skipSize) % state.list.size
      KnotState(newList, newPos, state.skipSize + 1)

  private def reverseSubList(list: IndexedSeq[Int],
                             start: Int,
                             length: Int): IndexedSeq[Int] =
    if length <= 0 then list
    else
      val size = list.size
      val indices = (0 until length).map(i => (start + i) % size)
      val values = indices.map(list)
      val reversedValues = values.reverse
      indices.zip(reversedValues).foldLeft(list):
        case (acc, (idx, value)) => acc.updated(idx, value)
  end reverseSubList

  def solvePart2(input: Seq[String]): String =
    val inputString = input.head.trim
    val lengths = (inputString.map(_.toInt) ++ Seq(17, 31, 73, 47, 23))
    val list = (0 to 255)

    val sparseHash = (0 until 64).foldLeft(KnotState(list)): (state, _) =>
      processRound(state, lengths)
    .list

    sparseHash.grouped(16)
      .map(_.reduce(_ ^ _))
      .map(n => f"$n%02x")
      .mkString
  end solvePart2

  private case class KnotState(list: IndexedSeq[Int], currentPos: Int = 0, skipSize: Int = 0)
end Day10