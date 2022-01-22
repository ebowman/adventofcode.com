package y2021

trait Day08 {

  def solve1(input: Seq[String]): Int =
    val inputs = input.map(_.split("\\|")(1).split("\\s+").filter(_.nonEmpty))
    val magic = Set(2, 3, 4, 7)
    inputs.flatMap(_.map(_.length)).count(magic)

  def solve2(input: Seq[String]): Int =
    val Pipe = """(.*?)\|(.*)""".r
    input.map {
      case Pipe(left, right) =>
        (left.split("""\s""").toSeq.filter(_.nonEmpty).map(_.sorted),
          right.split("""\s""").toSeq.filter(_.nonEmpty).map(_.sorted))
    }.map { case (nums, digits) =>
      val d1 = nums.find(_.length == 2).head
      val d4 = nums.find(_.length == 4).head
      val d7 = nums.find(_.length == 3).head
      val d8 = nums.find(_.length == 7).head
      val d9 = nums.find(n => n.length == 6 && d4.forall(n1 => n.contains(n1))).head
      val d0 = nums.find(n => n.length == 6 && n != d9 && d1.forall(n1 => n.contains(n1))).head
      val d6 = nums.find(n => n.length == 6 && n != d9 && n != d0).head
      val d3 = nums.find(n => n.length == 5 && d1.forall(n1 => n.contains(n1))).head
      val d5 = nums.find(n => n.length == 5 && n != d3 && n.forall(n1 => d9.contains(n1))).head
      val d2 = nums.find(n => n.length == 5 && n != d3 && n != d5).head
      val map = IndexedSeq(d0, d1, d2, d3, d4, d5, d6, d7, d8, d9)
      digits.map(map.indexOf).mkString.toInt
    }.sum
}