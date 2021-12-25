package y2021

trait Day24 {
  def solve(input: Seq[String]): (Long, Long) = {
    import collection.mutable
    val blocks = input.mkString("\n").split("inp w\n").toSeq.tail.map(_.split("\n").toSeq)
    val max = Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    val min = Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    val stack = mutable.Stack[(Int, Int)]()
    for {
      i <- blocks.indices
      block = blocks(i)
    } {
      block(3) match {
        case "div z 1" =>
          stack.push((i, block(14).split(' ').last.toInt))
        case "div z 26" =>
          val (j, x) = stack.pop()
          val diff = x + block(4).split(" ").last.toInt
          if (diff < 0) {
            max(j) = 9
            max(i) = 9 + diff
            min(j) = 1 - diff
            min(i) = 1
          } else {
            max(i) = 9
            max(j) = 9 - diff
            min(i) = 1 + diff
            min(j) = 1
          }
      }
    }
    (min.mkString.toLong, max.mkString.toLong)
  }
}
