package y2021

import scala.annotation.tailrec

trait Day24 {
  def solve(input: Seq[String]): (Long, Long) = {

    @tailrec def recurse(blocks: Seq[Seq[String]],
                         working: List[(Int, Int)] = Nil,
                         i: Int = 0,
                         min: Seq[Int] = Iterator.iterate(0, 14)(_ => 0).toSeq,
                         max: Seq[Int] = Iterator.iterate(0, 14)(_ => 0).toSeq): (Long, Long) = {
      if (blocks.isEmpty) (max.mkString.toLong, min.mkString.toLong)
      else blocks.head(3) match {
        case "div z 1" => recurse(blocks.tail, (i, blocks.head(14).split(' ').last.toInt) :: working, i + 1, min, max)
        case "div z 26" =>
          val (j, x) = working.head
          val diff = x + blocks.head(4).split(" ").last.toInt
          if (diff < 0) recurse(blocks.tail, working.tail, i + 1,
            min.updated(i, 9 + diff).updated(j, 9), max.updated(i, 1).updated(j, 1 - diff))
          else recurse(blocks.tail, working.tail, i + 1,
            min.updated(i, 9).updated(j, 9 - diff), max.updated(i, 1 + diff).updated(j, 1))
      }
    }

    recurse(input.mkString("\n").split("inp w\n").toSeq.tail.map(_.split("\n").toSeq))
  }
}
