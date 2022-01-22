package y2016

trait Day18 {

  case class TileRow(input: String) {
    def nextChar(x: Int): Char =
      s"${if x == 0 then "." else input(x - 1)}${input(x)}${if x == input.length - 1 then "." else input(x + 1)}" match {
        case "^^." | ".^^" | "^.." | "..^" => '^'
        case _ => '.'
      }

    def next: TileRow = TileRow(input.indices.map(nextChar).mkString)
  }

  def solve(input: String, n: Int): Int =
    Iterator.iterate(TileRow(input), n)(_.next).map(_.input.count(_ == '.')).sum
}
