package y2017

import scala.annotation.tailrec

trait Day09:

  enum State:
    case Normal, Garbage

  def solvePart1(input: String): Int =
    @tailrec def process(chars: List[Char], state: State = State.Normal, depth: Int = 0, score: Int = 0): Int =
      chars match
        case Nil => score
        case '!' :: tail => process(tail.tail, state, depth, score)
        case '>' :: tail if state == State.Garbage => process(tail, State.Normal, depth, score)
        case '<' :: tail if state == State.Normal => process(tail, State.Garbage, depth, score)
        case '{' :: tail if state == State.Normal => process(tail, State.Normal, depth + 1, score)
        case '}' :: tail if state == State.Normal => process(tail, State.Normal, depth - 1, score + depth)
        case _ :: tail if state == State.Garbage => process(tail, State.Garbage, depth, score)
        case _ :: tail => process(tail, state, depth, score)

    process(input.toList)

  def solvePart2(input: String): Int =
    @tailrec def process(chars: List[Char], state: State = State.Normal, garbageCount: Int = 0): Int =
      chars match
        case Nil => garbageCount
        case '!' :: tail => process(tail.tail, state, garbageCount)
        case '>' :: tail if state == State.Garbage => process(tail, State.Normal, garbageCount)
        case '<' :: tail if state == State.Normal => process(tail, State.Garbage, garbageCount)
        case _ :: tail if state == State.Garbage => process(tail, State.Garbage, garbageCount + 1)
        case _ :: tail => process(tail, state, garbageCount)

    process(input.toList)
end Day09