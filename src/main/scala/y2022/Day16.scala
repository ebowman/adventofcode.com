package y2022

trait Day16:
  case class Valve(name: String, flowRate: Int, tunnels: List[String])

  private def parseInput(input: Seq[String]): Map[String, Valve] =
    val pattern = """Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.+)""".r
    input.map:
      case pattern(name, rate, tunnels) =>
        name -> Valve(name, rate.toInt, tunnels.split(", ").toList)
    .toMap

  private def findShortestPaths(valves: Map[String, Valve]): Map[(String, String), Int] =
    val nodes = valves.keys.toList
    val inf = Int.MaxValue / 2

    val initial = (for
      i <- nodes
      j <- nodes
    yield (i, j) -> (
      if i == j then 0
      else if valves(i).tunnels.contains(j) then 1
      else inf
      )).toMap

    nodes.foldLeft(initial): (dist, k) =>
      nodes.foldLeft(dist): (d1, i) =>
        nodes.foldLeft(d1): (d2, j) =>
          val through_k = d2((i, k)) + d2((k, j))
          if through_k < d2((i, j)) then
            d2 + ((i, j) -> through_k)
          else d2
  end findShortestPaths

  private def findMaxPressure(valves: Map[String, Valve],
                              distances: Map[(String, String), Int],
                              timeLimit: Int): Int =
    val worthwhileValves = valves.filter(_._2.flowRate > 0)

    def calculateBestPressure(current: String,
                              timeLeft: Int,
                              remaining: Set[String],
                              accPressure: Int): Int =
      remaining.foldLeft(accPressure): (best, valve) =>
        val timeToMove = distances((current, valve)) + 1
        if timeToMove >= timeLeft then best
        else
          val newTimeLeft = timeLeft - timeToMove
          val addedPressure = newTimeLeft * worthwhileValves(valve).flowRate
          best.max(calculateBestPressure(
            valve,
            newTimeLeft,
            remaining - valve,
            accPressure + addedPressure
          ))

    calculateBestPressure("AA", timeLimit, worthwhileValves.keySet, 0)
  end findMaxPressure

  private def findMaxPressureWithElephant(valves: Map[String, Valve],
                                          distances: Map[(String, String), Int],
                                          timeLimit: Int): Int =
    val worthwhileValves = valves.filter(_._2.flowRate > 0)
    val valvesList = worthwhileValves.keys.toList
    val n = valvesList.size
    val allStates = 0 until (1 << n)

    def stateToValves(state: Int): Set[String] =
      valvesList.zipWithIndex.collect:
        case (valve, idx) if (state & (1 << idx)) != 0 => valve
      .toSet

    import collection.parallel.CollectionConverters.ImmutableSeqIsParallelizable
    val stateScores = allStates.par.map: state =>
      val valveSet = stateToValves(state)
      if valveSet.isEmpty then (state, 0)
      else
        def calculateBestPressure(current: String,
                                  timeLeft: Int,
                                  remaining: Set[String],
                                  accPressure: Int): Int =
          remaining.foldLeft(accPressure): (best, valve) =>
            val timeToMove = distances((current, valve)) + 1
            if timeToMove >= timeLeft then best
            else
              val newTimeLeft = timeLeft - timeToMove
              val addedPressure = newTimeLeft * worthwhileValves(valve).flowRate
              best.max(calculateBestPressure(
                valve,
                newTimeLeft,
                remaining - valve,
                accPressure + addedPressure
              ))

        (state, calculateBestPressure("AA", timeLimit, valveSet, 0))
    .toMap

    allStates.par.map: myState =>
      val elephantState = ((1 << n) - 1) ^ myState
      stateScores(myState) + stateScores(elephantState)
    .max
  end findMaxPressureWithElephant

  def solvePart1(input: Seq[String]): Int =
    val valves = parseInput(input)
    val distances = findShortestPaths(valves)
    findMaxPressure(valves, distances, 30)

  def solvePart2(input: Seq[String]): Int =
    val valves = parseInput(input)
    val distances = findShortestPaths(valves)
    findMaxPressureWithElephant(valves, distances, 26)

end Day16
