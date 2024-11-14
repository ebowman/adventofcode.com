package y2023

trait Day09 {
  def getDifferences(numbers: List[Long]): List[Long] = numbers.zip(numbers.tail).map { case (a, b) => b - a }

  def extrapolateNext(history: List[Long]): Long =
    if (history.forall(_ == 0)) return 0

    val differences = getDifferences(history)
    history.last + extrapolateNext(differences)

  def extrapolatePrevious(history: List[Long]): Long =
    if (history.isEmpty || history.forall(_ == 0)) return 0

    val differences = getDifferences(history)
    history.head - extrapolatePrevious(differences)

  def solvePart1(input: Seq[String]): Long =
    input
      .map(line => line.trim.split("\\s+").map(_.toLong).toList)
      .map(extrapolateNext)
      .sum

  def solvePart2(input: Seq[String]): Long =
    input
      .map(line => line.split("\\s+").map(_.toLong).toList)
      .map(sequence => {
        var current = sequence
        var sequences = List(sequence)
        while (!current.forall(_ == 0)) {
          current = getDifferences(current)
          sequences = current :: sequences
        }
        sequences.foldLeft(0L)((acc, seq) => seq.head - acc)
      })
      .sum
}