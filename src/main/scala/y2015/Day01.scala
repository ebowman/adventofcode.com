package y2015

trait Day01 {

  def solve1(input: String): Int =
    input.foldLeft(0) {
      case (a, '(') => a + 1
      case (a, _) => a - 1
    }

  def solve2(input: String): Int =
    input.foldLeft(0 :: Nil) {
      case (floors, '(') => (floors.head + 1) :: floors
      case (floors, _) => (floors.head - 1) :: floors
    }.reverse.indexOf(-1)
}
