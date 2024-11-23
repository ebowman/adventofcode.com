package y2022

trait Day20:
  case class Node(value: Long, originalIndex: Int)

  def solvePart1(input: Seq[String]): Long =
    val numbers = input.map(_.toLong)
    val mixed = mix(numbers)
    val result = getCoordinates(mixed)
    result

  private def mixOneRound(nodes: Seq[Node]): Seq[Node] =
    val len = nodes.length

    def moveNumber(list: Seq[Node], originalIndex: Int): Seq[Node] =
      val currentIndex = list.indexWhere(_.originalIndex == originalIndex)
      val node = list(currentIndex)

      if node.value == 0 then
        list
      else
        val withoutCurrent = list.take(currentIndex) ++ list.drop(currentIndex + 1)
        val remainingLen = len - 1

        // Calculate new position by walking the full number of steps
        val fullSteps = node.value
        val targetPos = (currentIndex.toLong + fullSteps) % remainingLen
        val newPos = ((targetPos % remainingLen + remainingLen) % remainingLen).toInt

        withoutCurrent.take(newPos) ++ Seq(node) ++ withoutCurrent.drop(newPos)

    (0 until len).foldLeft(nodes): (current, idx) =>
      moveNumber(current, idx)

  private def mix(numbers: Seq[Long]): Seq[Long] =
    val nodes = numbers.zipWithIndex.map((n, i) => Node(n, i))
    val result = mixOneRound(nodes)
    result.map(_.value)

  private def getCoordinateNumbers(numbers: Seq[Long]): Seq[Long] =
    val zeroIndex = numbers.indexOf(0)
    require(zeroIndex >= 0, "No zero found in sequence!")
    val len = numbers.length

    Seq(1000, 2000, 3000).map: steps =>
      val wrappedSteps = steps % len
      val index = (zeroIndex + wrappedSteps) % len
      numbers(index)

  private def getCoordinates(numbers: Seq[Long]): Long =
    val coords = getCoordinateNumbers(numbers)
    coords.sum

  def solvePart2(input: Seq[String]): Long =
    val decryptionKey = 811589153L
    val numbers = input.map(s => s.toLong * decryptionKey)
    val mixed = mixMultiple(numbers, rounds = 10)
    getCoordinates(mixed)

  private def mixMultiple(numbers: Seq[Long], rounds: Int): Seq[Long] =
    val nodes = numbers.zipWithIndex.map((n, i) => Node(n, i))
    val finalNodes = (0 until rounds).foldLeft(nodes): (current, _) =>
      mixOneRound(current)
    finalNodes.map(_.value)

end Day20