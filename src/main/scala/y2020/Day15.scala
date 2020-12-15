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
      case (_, Mask(m)) => parseMask(m)()
      case ((and, or), Write(addr, value)) => memory(addr.toInt) = (value.toLong & and) | or; (and, or)
    }

    memory.values.sum
  }

  // return (and, or)
  private final def parseMask(maskStr: String, shift: Int = 0)(and: Long = MASK, or: Long = 0): (Long, Long) = {
    if (maskStr.isEmpty) (and, or)
    else {
      val f = parseMask(maskStr.init, shift + 1) _
      maskStr.last match {
        case 'X' => f(and, or)
        case '0' => f(and & ~(1L << shift), or)
        case '1' => f(and, or | (1L << shift))
      }
    }
  }

}

trait Part2 extends Common {
  private type Op = Long => Long

  def part2(instructions: IndexedSeq[String]): Long = {
    val memory = allocateMem()

    instructions.foldLeft((0L, 0L)) {
      case (_, Mask(m)) => parseMask(m)()
      case ((float, or), Write(addr, value)) =>
        floatOps(or, float).map(_ (addr.toLong)).foreach(addr => memory(addr) = value.toLong); (float, or)
    }

    memory.values.sum
  }

  private def floatOps(orMask: Long, float: Long): Seq[Op] = {
    // which bits (e.g, 1, 2, 32, etc.) represent floats?
    val bits = (0 until 36).filter(bit => (float & (1L << bit)) != 0)

    // for each floating bit, we need to generate the rules to transform
    // an address into 'n' addresses (for 'n' floating bits). To do that, we build
    // up a sequence of lambda functions, 2 for each bit (one setting the bit on, one
    // setting it off). We then compose all those functions together and return them
    // together. We then push the address through each composed function, and set
    // the value at that address.
    // Note that each address is 'or'd with orMask to apply the 2nd rule ("1's are overwritten") (masks default arg)
    // The first rule ("0's are unchanged") is trivially satisfied.
    @tailrec
    def recurse(bits: Seq[Int], masks: Seq[Op] = Seq(_ | orMask, _ | orMask)): Seq[Op] = {
      if (bits.isEmpty) masks
      else {
        def or: Long => Long = _ | (1L << bits.head)

        def and: Long => Long = _ & ~(1L << bits.head)

        recurse(bits.tail, masks.flatMap(op => Seq(op compose or, op compose and)))
      }
    }

    recurse(bits)
  }

  // return (float, or)
  // Given a mask string, returns the "float" bits and the "or" bits.
  // For the float bits, we need to generate values at those bits that are both a 0 and a 1.
  // The or mask satisfies rule #2, "1s are overwritten". Rule #1 is trivial ("no change")
  private final def parseMask(maskStr: String, shift: Int = 0)(float: Long = 0, or: Long = 0): (Long, Long) = {
    if (maskStr.isEmpty) (float, or)
    else {
      val f = parseMask(maskStr.init, shift + 1) _
      maskStr.last match {
        case 'X' => f(float | (1L << shift), or)
        case '0' => f(float, or)
        case '1' => f(float, or | (1L << shift))
      }
    }
  }
}


trait Day14 extends Part1 with Part2