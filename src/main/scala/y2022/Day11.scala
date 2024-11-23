package y2022

case class Monkey(
                   id: Int,
                   items: List[Long],
                   operation: Long => Long,
                   testDivisor: Long,
                   trueMonkey: Int,
                   falseMonkey: Int,
                   inspectionCount: Long = 0
                 )

trait Day11:
  private def parseMonkey(input: Seq[String]): Monkey =
    val id = input.head.drop(7).dropRight(1).toInt
    val items = input(1).drop(18).split(", ").map(_.toLong).toList
    val op = input(2).drop(23).split(" ") match
      case Array("*", "old") => (old: Long) => old * old
      case Array("*", n) => (old: Long) => old * n.toLong
      case Array("+", n) => (old: Long) => old + n.toLong
    val testDiv = input(3).drop(21).toLong
    val trueMonkey = input(4).drop(29).toInt
    val falseMonkey = input(5).drop(30).toInt
    Monkey(id, items, op, testDiv, trueMonkey, falseMonkey)

  def parseInput(input: Seq[String]): Vector[Monkey] =
    input.filterNot(_.isEmpty)
      .grouped(6)
      .map(parseMonkey)
      .toVector

  def processRound(monkeys: Vector[Monkey], worryReducer: Long => Long): Vector[Monkey] =
    monkeys.indices.foldLeft(monkeys): (currentMonkeys, currentId) =>
      val monkey = currentMonkeys(currentId)
      monkey.items.foldLeft(currentMonkeys): (mks, item) =>
        val newWorry = worryReducer(monkey.operation(item))
        val targetMonkey = if newWorry % monkey.testDivisor == 0 then
          monkey.trueMonkey
        else
          monkey.falseMonkey
        mks.updated(targetMonkey,
            mks(targetMonkey).copy(items = mks(targetMonkey).items :+ newWorry))
          .updated(currentId,
            mks(currentId).copy(
              items = if mks(currentId).items.isEmpty then List() else mks(currentId).items.tail,
              inspectionCount = mks(currentId).inspectionCount + 1
            ))

  private def calculateMonkeyBusiness(monkeys: Vector[Monkey]): Long =
    monkeys.map(_.inspectionCount).sorted.takeRight(2).product

  def solvePart1(input: Seq[String]): Long =
    val monkeys = parseInput(input)
    val finalState = (1 to 20).foldLeft(monkeys): (mks, _) =>
      processRound(mks, worry => worry / 3)
    calculateMonkeyBusiness(finalState)

  def solvePart2(input: Seq[String]): Long =
    val monkeys = parseInput(input)
    val modulo = monkeys.map(_.testDivisor).product
    val finalState = (1 to 10000).foldLeft(monkeys): (mks, _) =>
      processRound(mks, worry => worry % modulo)
    calculateMonkeyBusiness(finalState)
end Day11
