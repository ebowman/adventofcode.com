package y2021

trait Day10 {
  val opens = Set('(', '<', '{', '[')
  val matcher = Map(')' -> '(', '>' -> '<', '}' -> '{', ']' -> '[')

  def solve1(input: Seq[String]): Long = {
    val scores = Map(')' -> 3L, ']' -> 57L, '}' -> 1197L, '>' -> 25137L)

    def recurse(stack: List[Char] = Nil)(i: String): Long =
      if (i.isEmpty) 0
      else if (opens.contains(i.head)) recurse(i.head :: stack)(i.tail)
      else if (matcher(i.head) == stack.head) recurse(stack.tail)(i.tail)
      else scores(i.head)

    input.map(recurse()).sum
  }

  def solve2(input: Seq[String]): Long = {
    val scoreMap = Map('(' -> 1L, '[' -> 2L, '{' -> 3L, '<' -> 4L)

    def recurse(stack: List[Char] = Nil)(i: String): String =
      if (i.isEmpty) stack.mkString
      else if (opens.contains(i.head)) recurse(i.head :: stack)(i.tail)
      else if (matcher(i.head) == stack.head) recurse(stack.tail)(i.tail)
      else ""

    def score(str: String): Long = str.foldLeft(0L) { case (score, letter) => 5L * score + scoreMap(letter) }

    val scores = input.map(recurse()).filter(_.nonEmpty).map(score).sorted
    scores(scores.size / 2)
  }
}