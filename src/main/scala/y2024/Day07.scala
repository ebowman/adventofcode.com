package y2024

class Day07 extends util.Day(7):
  enum Operator:
    case Add, Multiply, Concat

    def apply(a: Long, b: Long): Long = this match
      case Add => a + b
      case Multiply => a * b
      case Concat => s"$a$b".toLong
  end Operator

  case class Equation(target: Long, numbers: List[Long])

  private def parseInput(input: IndexedSeq[String]): List[Equation] =
    input.map : line =>
      val Array(target, nums) = line.split(": ")
      Equation(
        target.toLong,
        nums.trim.split(" ").map(_.toLong).toList
      )
    .toList

  private def generateCombinations(n: Int, operators: List[Operator]): List[List[Operator]] =
    if n <= 0 then List(Nil)
    else
      for
        ops <- generateCombinations(n - 1, operators)
        op <- operators
      yield op :: ops

  private def evaluate(numbers: List[Long], operators: List[Operator]): Long =
    operators.zip(numbers.tail).foldLeft(numbers.head):
      case (acc, (op, num)) =>
         op(acc, num)

  private def canMakeTarget(eq: Equation, operators: List[Operator]): Boolean =
    val numOperators = eq.numbers.size - 1
    generateCombinations(numOperators, operators).exists: operators =>
      evaluate(eq.numbers, operators) == eq.target

  def solvePart1(input: IndexedSeq[String]): Long =
    val equations = parseInput(input)
    equations
      .filter(canMakeTarget(_, List(Operator.Add, Operator.Multiply)))
      .map(_.target)
      .sum

  def solvePart2(input: IndexedSeq[String]): Long =
    val equations = parseInput(input)
    equations
      .filter(canMakeTarget(_, List(Operator.Add, Operator.Multiply, Operator.Concat)))
      .map(_.target)
      .sum
end Day07
