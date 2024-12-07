package y2024

import scala.annotation.tailrec

class Day07 extends util.Day(7):
  def solvePart1(input: IndexedSeq[String]): Long =
    solve(input, List(Operator.Add, Operator.Multiply))

  def solvePart2(input: IndexedSeq[String]): Long =
    solve(input, List(Operator.Add, Operator.Multiply, Operator.Concat))

  private def solve(input: IndexedSeq[String], operators: List[Operator]): Long =
    val equations = parseInput(input)
    equations
      .filter(canMakeTarget(_, operators))
      .map(_.target)
      .sum

  private def parseInput(input: IndexedSeq[String]): List[Equation] =
    input.map: line =>
      val Array(target, nums) = line.split(": ")
      Equation(
        target.toLong,
        nums.trim.split(" ").map(_.toLong).toList)
    .toList

  private def generateCombinations(n: Int, operators: List[Operator]): List[List[Operator]] =
    if n <= 0 then List(Nil)
    else
      for
        ops <- generateCombinations(n - 1, operators)
        op <- operators
      yield op :: ops

  private def evaluate(numbers: List[Long], operators: List[Operator]): Long =
    operators.iterator.zip(numbers.tail).foldLeft(numbers.head):
      case (acc, (op, num)) =>
        op(acc, num)

  private def canMakeTarget(eq: Equation, operators: List[Operator]): Boolean =
    val numOperators = eq.numbers.size - 1
    generateCombinations(numOperators, operators).exists: operators =>
      evaluate(eq.numbers, operators) == eq.target

  private enum Operator:
    case Add, Multiply, Concat

    def apply(a: Long, b: Long): Long = this match
      case Add => a + b
      case Multiply => a * b
      case Concat =>
        def countDigits(n: Long): Int =
          if n == 0 then 1
          else
            @tailrec def recurse(x: Long, count: Int = 0): Int =
              if (x == 0) count else recurse(x / 10, count + 1)

            recurse(n)
        end countDigits

        def pow10(exp: Int): Long =
          @tailrec def recurse(e: Int, acc: Long = 1L): Long =
            if e == 0 then acc else recurse(e - 1, acc * 10)

          recurse(exp)
        end pow10

        val digits = countDigits(b)
        a * pow10(digits) + b
    end apply
  end Operator

  private case class Equation(target: Long, numbers: List[Long])
end Day07
