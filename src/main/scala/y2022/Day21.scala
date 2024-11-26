package y2022

import scala.annotation.tailrec

trait Day21:
  enum Operation:
    case Add, Subtract, Multiply, Divide, Equals

    def apply(left: Long, right: Long): Long = this match
      case Add => left + right
      case Subtract => left - right
      case Multiply => left * right
      case Divide => left / right
      case Equals => if left == right then left else throw IllegalStateException("Not equal")

    def inverse(result: Long, known: Long, knownIsLeft: Boolean): Long = this match
      case Add => result - known
      case Multiply => result / known
      case Subtract if knownIsLeft => known - result
      case Subtract => result + known
      case Divide if knownIsLeft => known / result
      case Divide => result * known
      case Equals => known
  end Operation

  enum Job:
    case Number(value: Long)
    case Expression(left: String, op: Operation, right: String)
  end Job

  private case class Monkey(name: String, job: Job)

  private def parseOperation(op: String): Operation = op.trim match
    case "+" => Operation.Add
    case "-" => Operation.Subtract
    case "*" => Operation.Multiply
    case "/" => Operation.Divide
    case "=" => Operation.Equals
    case invalid => throw IllegalArgumentException(s"Unknown operation: $invalid")

  private def parseLine(forPart2: Boolean)(line: String): Monkey =
    val Array(name, jobStr) = line.split(":")
    val trimmedName = name.trim
    val trimmedJob = jobStr.trim

    val job = trimmedJob match
      case _ if trimmedName == "humn" && forPart2 =>
        Job.Number(0) // placeholder, won't be used
      case s if trimmedName == "root" && forPart2 =>
        val Array(left, _, right) = s.split(" ")
        Job.Expression(left, Operation.Equals, right)
      case number if number.matches("""\d+""") =>
        Job.Number(number.toLong)
      case expr =>
        val Array(left, op, right) = expr.split(" ")
        Job.Expression(left, parseOperation(op), right)

    Monkey(trimmedName, job)
  end parseLine

  private def evaluatePart1(monkeyName: String,
                            monkeys: Map[String, Monkey]): Long =
    monkeys(monkeyName).job match
      case Job.Number(value) => value
      case Job.Expression(left, op, right) =>
        val leftValue = evaluatePart1(left, monkeys)
        val rightValue = evaluatePart1(right, monkeys)
        op.apply(leftValue, rightValue)

  private def evaluatePart2(monkeyName: String,
                            monkeys: Map[String, Monkey]): Option[Long] =
    if monkeyName == "humn" then None
    else monkeys(monkeyName).job match
      case Job.Number(value) => Some(value)
      case Job.Expression(left, op, right) =>
        val leftValue = evaluatePart2(left, monkeys)
        val rightValue = evaluatePart2(right, monkeys)

        (leftValue, rightValue) match
          case (Some(l), Some(r)) => Some(op.apply(l, r))
          case _ => None

  @tailrec
  private def solveForHuman(monkeyName: String,
                            targetValue: Long,
                            monkeys: Map[String, Monkey]): Long =
    if monkeyName == "humn" then targetValue
    else
      monkeys(monkeyName).job match
        case Job.Expression(left, op, right) =>
          val leftValue = evaluatePart2(left, monkeys)
          val rightValue = evaluatePart2(right, monkeys)

          (leftValue, rightValue) match
            case (None, Some(right)) =>
              solveForHuman(left, op.inverse(targetValue, right, false), monkeys)
            case (Some(left), None) =>
              solveForHuman(right, op.inverse(targetValue, left, true), monkeys)
            case _ => throw IllegalStateException("Invalid state")
        case _ => throw IllegalStateException("Expected expression")
  end solveForHuman

  def solvePart1(input: Seq[String]): Long =
    val monkeys = input.map(parseLine(forPart2 = false)).map(m => m.name -> m).toMap
    evaluatePart1("root", monkeys)

  def solvePart2(input: Seq[String]): Long =
    val monkeys = input.map(parseLine(forPart2 = true)).map(m => m.name -> m).toMap
    val Monkey(_, Job.Expression(left, _, right)) = monkeys("root"): @unchecked

    (evaluatePart2(left, monkeys), evaluatePart2(right, monkeys)) match
      case (None, Some(right)) => solveForHuman(left, right, monkeys)
      case (Some(left), None) => solveForHuman(right, left, monkeys)
      case _ => throw IllegalStateException("Invalid state")
      
end Day21