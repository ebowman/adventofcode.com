package y2017

import scala.annotation.tailrec
import scala.collection.immutable.Vector

trait Day17:
  def solvePart1(steps: Int): Int =

    case class State(buffer: Vector[Int], position: Int, nextValue: Int)

    def step(state: State): State =
      // Calculate new position after moving forward
      val bufferSize = state.buffer.size
      val newPosition = ((state.position + steps) % bufferSize) + 1

      // Insert new value at the new position
      val (before, after) = state.buffer.splitAt(newPosition)
      val newBuffer = before ++ Vector(state.nextValue) ++ after

      State(buffer = newBuffer, position = newPosition, nextValue = state.nextValue + 1)
    end step

    // Initial state with just 0
    val initial = State(Vector(0), 0, 1)

    // Run the simulation 2017 times
    val finalState = (1 to 2017).foldLeft(initial)((state, _) => step(state))

    // Find the value after 2017
    val pos2017 = finalState.buffer.indexOf(2017)
    val nextPos = (pos2017 + 1) % finalState.buffer.size
    finalState.buffer(nextPos)

  def solvePart2(steps: Int): Int =

    // For part 2, we only need to track what's at position 1
    // since we're looking for values after position 0
    def simulatePosition1(iterations: Int): Int =
      @tailrec
      def recurse(pos: Int = 0, valueAtPos1: Int = 0, currentSize: Int = 1, iteration: Int = 1): Int =
        if iteration > iterations then valueAtPos1
        else
          val newPos = ((pos + steps) % currentSize) + 1
          val newValueAtPos1 =
            if newPos == 1 then iteration
            else valueAtPos1

          recurse(newPos, newValueAtPos1, currentSize + 1, iteration + 1)
      end recurse

      recurse()
    end simulatePosition1

    simulatePosition1(50_000_000)
end Day17