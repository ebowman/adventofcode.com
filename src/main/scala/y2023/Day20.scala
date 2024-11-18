package y2023

import scala.annotation.tailrec

// see https://adventofcode.com/2023/day/20
trait Day20:
  def solvePart1(input: Seq[String]): Long =
    var modules = parseInput(input)
    var totalLow = 0L
    var totalHigh = 0L

    for _ <- 1 to 1000 do
      val (newModules, low, high) = pressButton(modules)
      modules = newModules
      totalLow += low
      totalHigh += high

    totalLow * totalHigh

  def solvePart2(input: Seq[String]): Long =
    var modules = parseInput(input)

    // Find the module that feeds into rx
    val rxInput = modules.find(_._2.destinations.contains("rx")).get._1

    // Find all modules that feed into the rx input (these are likely conjunction modules)
    val rxInputFeeds = modules.filter(_._2.destinations.contains(rxInput)).keySet

    // Track when each input sends a high pulse
    var cycleDetection = rxInputFeeds.map(_ -> 0L).toMap
    var buttonPresses = 0L

    // We need all inputs to send high pulses to the conjunction module feeding rx
    // to get a low pulse output. Find the cycle length for each input.
    while cycleDetection.values.exists(_ == 0) do
      buttonPresses += 1
      val (newModules, messages) = pressButtonWithHistory(modules)
      modules = newModules

      // Check which inputs sent high pulses this round
      for
        msg <- messages
        if msg.to == rxInput && msg.pulse == Pulse.High
        if cycleDetection(msg.from) == 0
      do
        cycleDetection = cycleDetection.updated(msg.from, buttonPresses)

    // The answer is the least common multiple of all cycle lengths
    cycleDetection.values.fold(1L)(lcm)

  private def lcm(a: Long, b: Long): Long =
    @tailrec def gcd(x: Long, y: Long): Long = if y == 0 then x else gcd(y, x % y)
    a * (b / gcd(a, b))

  private def pressButtonWithHistory(modules: Map[String, Module]): (Map[String, Module], List[PulseMessage]) =
    var currentModules = modules
    var queue = List(PulseMessage("button", "broadcaster", Pulse.Low))
    var allMessages = List[PulseMessage]()

    while queue.nonEmpty do
      val msg = queue.head
      queue = queue.tail
      allMessages = msg :: allMessages

      currentModules.get(msg.to).foreach { module =>
        val (newModule, newPulses) = processModule(module, msg)
        currentModules = currentModules.updated(module.name, newModule)
        queue = queue ++ newPulses
      }

    (currentModules, allMessages.reverse)

  def parseInput(input: Seq[String]): Map[String, Module] =
    // First pass: Create basic modules
    val modules = input.map { line =>
      val parts = line.split("->").map(_.trim)
      val (moduleType, name) = parts(0) match
        case "broadcaster" => (ModuleType.Broadcaster, "broadcaster")
        case s if s.startsWith("%") => (ModuleType.FlipFlop, s.tail)
        case s if s.startsWith("&") => (ModuleType.Conjunction, s.tail)
        case s => throw IllegalArgumentException(s"Invalid module: $s")

      val destinations = parts(1).split(",").map(_.trim).toList
      name -> Module(name, moduleType, destinations)
    }.toMap

    // Second pass: Initialize conjunction module memories
    val withMemory = modules.map { case (name, module) =>
      val inputs = modules.collect {
        case (srcName, srcModule) if srcModule.destinations.contains(name) => srcName
      }

      if module.moduleType == ModuleType.Conjunction then
        name -> module.copy(memory = inputs.map(_ -> Pulse.Low).toMap)
      else
        name -> module
    }

    withMemory

  private def pressButton(modules: Map[String, Module]): (Map[String, Module], Long, Long) =
    var currentModules = modules
    var lowCount = 1L // Start with 1 for button press
    var highCount = 0L
    var queue = List(PulseMessage("button", "broadcaster", Pulse.Low))

    while queue.nonEmpty do
      val msg = queue.head
      queue = queue.tail

      currentModules.get(msg.to).foreach { module =>
        val (newModule, newPulses) = processModule(module, msg)
        currentModules = currentModules.updated(module.name, newModule)
        queue = queue ++ newPulses

        // Count the new pulses
        newPulses.foreach(p =>
          if p.pulse == Pulse.Low then lowCount += 1 else highCount += 1
        )
      }

    (currentModules, lowCount, highCount)

  private def processModule(module: Module, msg: PulseMessage): (Module, List[PulseMessage]) =
    module.moduleType match
      case ModuleType.Broadcaster =>
        (module, module.destinations.map(dest =>
          PulseMessage(module.name, dest, msg.pulse)))

      case ModuleType.FlipFlop =>
        msg.pulse match
          case Pulse.High => (module, Nil)
          case Pulse.Low =>
            val newState = !module.state
            val newPulse = if newState then Pulse.High else Pulse.Low
            (module.copy(state = newState),
              module.destinations.map(dest =>
                PulseMessage(module.name, dest, newPulse)))

      case ModuleType.Conjunction =>
        val newMemory = module.memory.updated(msg.from, msg.pulse)
        val outputPulse = if newMemory.values.forall(_ == Pulse.High) then Pulse.Low else Pulse.High
        (module.copy(memory = newMemory),
          module.destinations.map(dest =>
            PulseMessage(module.name, dest, outputPulse)))

  enum Pulse:
    case High, Low

  enum ModuleType:
    case FlipFlop, Conjunction, Broadcaster

  case class Module( name: String,
                     moduleType: ModuleType,
                     destinations: List[String],
                     state: Boolean = false, // For flip-flops: on/off
                     memory: Map[String, Pulse] = Map.empty) // For conjunctions: remembers input states

  private case class PulseMessage(from: String, to: String, pulse: Pulse)

end Day20