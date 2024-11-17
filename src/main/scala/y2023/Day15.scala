package y2023

// see https://adventofcode.com/2023/day/15
trait Day15 {
  def solvePart1(input: String): Int =
    input.trim
      .split(",")
      .map(hash)
      .sum

  def solvePart2(input: String): Int = {
    val boxes = Array.fill[List[Lens]](256)(List.empty)

    input.trim.split(",").foreach { step =>
      if (step.contains('=')) {
        val Array(label, focalLength) = step.split("=")
        val boxNum = hash(label)
        val lens = Lens(label, focalLength.toInt)

        boxes(boxNum) = boxes(boxNum) match {
          case existing if existing.exists(_.label == label) =>
            // Replace existing lens
            existing.map(l => if (l.label == label) lens else l)
          case existing =>
            // Add new lens to the end
            existing :+ lens
        }
      } else {
        // Remove operation
        val label = step.dropRight(1) // Remove the '-'
        val boxNum = hash(label)
        boxes(boxNum) = boxes(boxNum).filterNot(_.label == label)
      }
    }

    // Calculate focusing power
    boxes.zipWithIndex.map { case (lenses, boxNum) =>
      lenses.zipWithIndex.map { case (lens, slot) =>
        (boxNum + 1) * (slot + 1) * lens.focalLength
      }.sum
    }.sum
  }

  def hash(s: String): Int =
    s.foldLeft(0) { (currentValue, char) =>
      ((currentValue + char.toInt) * 17) % 256
    }

  case class Lens(label: String, focalLength: Int)
}
