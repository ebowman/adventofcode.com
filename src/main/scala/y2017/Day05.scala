package y2017

trait Day05 {

  def compute(data: Array[Int], modifier: Int => Int): Int =
    var steps = 0
    var cursor = 0
    while (cursor >= 0 && cursor < data.length) {
      val newCursor = cursor + data(cursor)
      data(cursor) += modifier(data(cursor))
      cursor = newCursor
      steps += 1
    }
    steps

  def solve1(input: Seq[String]): Int = compute(input.map(_.toInt).toArray, _ => 1)

  def solve2(input: Seq[String]): Int = compute(input.map(_.toInt).toArray, x => if x >= 3 then -1 else 1)
}
