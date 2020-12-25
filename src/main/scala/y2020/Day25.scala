package y2020

import scala.annotation.tailrec

trait Day25 {

  @tailrec final def transform(loopSize: Long, subject: Long, accum: Long = 1): Long = {
    if (loopSize == 0) accum
    else transform(loopSize - 1, subject, (accum * subject) % 20201227)
  }

  case class Entity(secretLoopSize: Long) {
    def publicKey: Long = transform(loopSize = secretLoopSize, 7)

    def encryptionKey(other: Entity): Long = transform(secretLoopSize, other.publicKey)
  }

  def findKey(publicKey: Long): Entity = {
    @tailrec def recurse(loop: Long = 1, accum: Long = 1): Long = {
      val key = (accum * 7) % 20201227
      if (key == publicKey) loop
      else recurse(loop + 1, key)
    }

    Entity(recurse())
  }

  def part1(input: IndexedSeq[String]): Long = findKey(input(0).toLong).encryptionKey(findKey(input(1).toLong))
}
