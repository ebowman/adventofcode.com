package y2020

import scala.annotation.tailrec
import scala.util.matching.Regex

trait Common {
  val MASK: Long = (1L << 36) - 1
  val Mask: Regex = """mask = (.*)""".r
  val Write: Regex = """mem\[(.*?)] = (.*)""".r

  def allocateMem(): collection.mutable.Map[Long, Long] = {
    new collection.mutable.HashMap[Long, Long]() {
      override def default(key: Long): Long = 0L
    }
  }
}

trait Part1 extends Common {
  def part1(instructions: IndexedSeq[String]): Long = {
    val memory = allocateMem()

    instructions.foldLeft((0L, 0L)) {
      case (_, Mask(m)) => parseMask(m)
      case ((and, or), Write(addr, value)) => memory(addr.toInt) = (value.toLong & and) | or; (and, or)
    }

    memory.values.sum
  }

  // return (and, or)
  @tailrec
  private final def parseMask(maskStr: String, shift: Int = 0, and: Long = MASK, or: Long = 0): (Long, Long) = {
    if (maskStr.isEmpty) (and, or)
    else maskStr.last match {
      case 'X' => parseMask(maskStr.init, shift + 1, and, or)
      case '0' => parseMask(maskStr.init, shift + 1, and & ~(1L << shift), or)
      case '1' => parseMask(maskStr.init, shift + 1, and, or | (1L << shift))
    }
  }

}

trait Part2 extends Common {
  def part2(instructions: IndexedSeq[String]): Long = {
    val memory = allocateMem()

    instructions.foldLeft((0L, 0L)) {
      case (_, Mask(m)) => parseMask(m)
      case ((float, or), Write(addr, value)) =>
        floatOps(float).map(_ (or | addr.toLong)).foreach(addr => memory(addr) = value.toLong); (float, or)
    }

    memory.values.sum
  }

  // return (float, or)
  @tailrec
  private final def parseMask(maskStr: String, shift: Int = 0, float: Long = 0, or: Long = 0): (Long, Long) = {
    if (maskStr.isEmpty) (float, or)
    else maskStr.last match {
      case 'X' => parseMask(maskStr.init, shift + 1, float | (1L << shift), or)
      case '0' => parseMask(maskStr.init, shift + 1, float, or)
      case '1' => parseMask(maskStr.init, shift + 1, float, or | (1L << shift))
    }
  }

  private type Op = Long => Long

  private def floatOps(float: Long): Seq[Op] = {
    val bits = (0 until 36).filter(bit => (float & (1L << bit)) != 0)

    @tailrec
    def recurse(bits: Seq[Int], masks: Seq[Op] = Seq(identity, identity)): Seq[Op] = {
      if (bits.isEmpty) masks
      else {
        def or: Long => Long = _ | (1L << bits.head)
        def and: Long => Long = _ & ~(1L << bits.head)
        recurse(bits.tail, masks.flatMap(op => Seq(op compose or, op compose and)))
      }
    }

    recurse(bits)
  }
}


trait Day14 extends Part1 with Part2