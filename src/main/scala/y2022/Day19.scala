package y2022

case class Blueprint(id: Int,
                     oreRobotCost: Int,
                     clayRobotCost: Int,
                     obsidianRobotCost: (Int, Int),
                     geodeRobotCost: (Int, Int))

// Packed representation for better memory usage and faster comparison
// Uses a single Long to store all state information
case class State(packed: Long):
  def ore: Int = ((packed >> 52) & 0x3F).toInt
  def clay: Int = ((packed >> 44) & 0xFF).toInt
  def obsidian: Int = ((packed >> 36) & 0xFF).toInt
  def geodes: Int = ((packed >> 28) & 0xFF).toInt
  def oreRobots: Int = ((packed >> 24) & 0xF).toInt
  def clayRobots: Int = ((packed >> 20) & 0xF).toInt
  def obsidianRobots: Int = ((packed >> 16) & 0xF).toInt
  def geodeRobots: Int = ((packed >> 12) & 0xF).toInt
  def timeLeft: Int = (packed & 0x3F).toInt

object State:
  def apply(ore: Int = 0,
            clay: Int = 0,
            obsidian: Int = 0,
            geodes: Int = 0,
            oreRobots: Int = 1,
            clayRobots: Int = 0,
            obsidianRobots: Int = 0,
            geodeRobots: Int = 0,
            timeLeft: Int): State =
    new State(
      ((ore.toLong & 0x3F) << 52) |
        ((clay.toLong & 0xFF) << 44) |
        ((obsidian.toLong & 0xFF) << 36) |
        ((geodes.toLong & 0xFF) << 28) |
        ((oreRobots.toLong & 0xF) << 24) |
        ((clayRobots.toLong & 0xF) << 20) |
        ((obsidianRobots.toLong & 0xF) << 16) |
        ((geodeRobots.toLong & 0xF) << 12) |
        (timeLeft.toLong & 0x3F)
    )

trait Day19:
  def parseBlueprint(input: String): Blueprint =
    val numbers = """\d+""".r.findAllIn(input).map(_.toInt).toList
    Blueprint(
      id = numbers.head,
      oreRobotCost = numbers(1),
      clayRobotCost = numbers(2),
      obsidianRobotCost = (numbers(3), numbers(4)),
      geodeRobotCost = (numbers(5), numbers(6))
    )

  def simulate(blueprint: Blueprint, initialTime: Int): Int =
    val maxOreNeeded = List(
      blueprint.oreRobotCost,
      blueprint.clayRobotCost,
      blueprint.obsidianRobotCost._1,
      blueprint.geodeRobotCost._1
    ).max

    var bestSoFar = 0
    val cache = collection.mutable.HashSet[Long]()

    def dfs(state: State): Unit =
      if state.timeLeft == 0 then
        bestSoFar = bestSoFar.max(state.geodes)
        return

      // Prune if we can't possibly beat the best score
      val maxPossibleNew = state.geodes +
        state.geodeRobots * state.timeLeft +
        (state.timeLeft * (state.timeLeft - 1)) / 2

      if maxPossibleNew <= bestSoFar || !cache.add(state.packed) then return

      // Try building geode robot
      if state.ore >= blueprint.geodeRobotCost._1 && state.obsidian >= blueprint.geodeRobotCost._2 then
        dfs(State(
          ore = state.ore + state.oreRobots - blueprint.geodeRobotCost._1,
          clay = state.clay + state.clayRobots,
          obsidian = state.obsidian + state.obsidianRobots - blueprint.geodeRobotCost._2,
          geodes = state.geodes + state.geodeRobots,
          oreRobots = state.oreRobots,
          clayRobots = state.clayRobots,
          obsidianRobots = state.obsidianRobots,
          geodeRobots = state.geodeRobots + 1,
          timeLeft = state.timeLeft - 1
        ))
        return

      // Try building obsidian robot
      if state.ore >= blueprint.obsidianRobotCost._1 &&
        state.clay >= blueprint.obsidianRobotCost._2 &&
        state.obsidianRobots < blueprint.geodeRobotCost._2 then
        dfs(State(
          ore = state.ore + state.oreRobots - blueprint.obsidianRobotCost._1,
          clay = state.clay + state.clayRobots - blueprint.obsidianRobotCost._2,
          obsidian = state.obsidian + state.obsidianRobots,
          geodes = state.geodes + state.geodeRobots,
          oreRobots = state.oreRobots,
          clayRobots = state.clayRobots,
          obsidianRobots = state.obsidianRobots + 1,
          geodeRobots = state.geodeRobots,
          timeLeft = state.timeLeft - 1
        ))

      // Try building clay robot
      if state.ore >= blueprint.clayRobotCost &&
        state.clayRobots < blueprint.obsidianRobotCost._2 then
        dfs(State(
          ore = state.ore + state.oreRobots - blueprint.clayRobotCost,
          clay = state.clay + state.clayRobots,
          obsidian = state.obsidian + state.obsidianRobots,
          geodes = state.geodes + state.geodeRobots,
          oreRobots = state.oreRobots,
          clayRobots = state.clayRobots + 1,
          obsidianRobots = state.obsidianRobots,
          geodeRobots = state.geodeRobots,
          timeLeft = state.timeLeft - 1
        ))

      // Try building ore robot
      if state.ore >= blueprint.oreRobotCost &&
        state.oreRobots < maxOreNeeded then
        dfs(State(
          ore = state.ore + state.oreRobots - blueprint.oreRobotCost,
          clay = state.clay + state.clayRobots,
          obsidian = state.obsidian + state.obsidianRobots,
          geodes = state.geodes + state.geodeRobots,
          oreRobots = state.oreRobots + 1,
          clayRobots = state.clayRobots,
          obsidianRobots = state.obsidianRobots,
          geodeRobots = state.geodeRobots,
          timeLeft = state.timeLeft - 1
        ))

      // Do nothing
      dfs(State(
        ore = state.ore + state.oreRobots,
        clay = state.clay + state.clayRobots,
        obsidian = state.obsidian + state.obsidianRobots,
        geodes = state.geodes + state.geodeRobots,
        oreRobots = state.oreRobots,
        clayRobots = state.clayRobots,
        obsidianRobots = state.obsidianRobots,
        geodeRobots = state.geodeRobots,
        timeLeft = state.timeLeft - 1
      ))

    dfs(State(timeLeft = initialTime))
    bestSoFar

  def solvePart1(input: Seq[String]): Int =
    input.map(parseBlueprint)
      .map(bp => bp.id * simulate(bp, 24))
      .sum

  def solvePart2(input: Seq[String]): Int =
    input.take(3).map(parseBlueprint)
      .map(simulate(_, 32))
      .product
end Day19
