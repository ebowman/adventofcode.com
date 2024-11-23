package y2022

trait Day17:
  enum Rock:
    case Horizontal, Plus, Corner, Vertical, Square

    def shape: Set[(Int, Int)] = this match
      case Horizontal => Set((0, 0), (1, 0), (2, 0), (3, 0))
      case Plus => Set((1, 0), (0, 1), (1, 1), (2, 1), (1, 2))
      case Corner => Set((0, 0), (1, 0), (2, 0), (2, 1), (2, 2))
      case Vertical => Set((0, 0), (0, 1), (0, 2), (0, 3))
      case Square => Set((0, 0), (1, 0), (0, 1), (1, 1))

  val width = 7
  val startX = 2
  val startY = 3

  case class Chamber(rocks: Set[(Int, Int)] = Set.empty,
                     currentHeight: Int = 0,
                     rockIndex: Int = 0,
                     jetIndex: Int = 0)

  case class State(rockIndex: Int, jetIndex: Int, profile: String)
  case class CycleInfo(rockCount: Long, height: Long)

  def getRock(index: Int): Rock =
    Rock.values(index % Rock.values.length)

  def moveRock(rock: Set[(Int, Int)], dx: Int, dy: Int): Set[(Int, Int)] =
    rock.map((x, y) => (x + dx, y + dy))

  def isValid(rock: Set[(Int, Int)], chamber: Chamber): Boolean =
    rock.forall: (x, y) =>
      x >= 0 && x < width && y >= 0 && !chamber.rocks.contains((x, y))

  def getProfile(chamber: Chamber, depth: Int = 30): String =
    val maxHeight = chamber.currentHeight
    val profile = for
      y <- (maxHeight - depth).max(0) until maxHeight
      x <- 0 until width
    yield
      if chamber.rocks.contains((x, y)) then '#' else '.'
    profile.mkString

  def dropRock(chamber: Chamber, jets: String): Chamber =
    val rock = getRock(chamber.rockIndex).shape
    var pos = (startX, chamber.currentHeight + startY)
    var currentJetIndex = chamber.jetIndex

    def moveJet(): Unit =
      val dx = if jets(currentJetIndex % jets.length) == '<' then -1 else 1
      currentJetIndex += 1
      val movedRock = moveRock(rock, dx, 0).map((x, y) => (x + pos._1, y + pos._2))
      if isValid(movedRock, chamber) then
        pos = (pos._1 + dx, pos._2)

    def moveDown(): Boolean =
      val movedRock = moveRock(rock, 0, -1).map((x, y) => (x + pos._1, y + pos._2))
      if isValid(movedRock, chamber) then
        pos = (pos._1, pos._2 - 1)
        true
      else
        false

    var moving = true
    while moving do
      moveJet()
      moving = moveDown()

    val finalRock = rock.map((x, y) => (x + pos._1, y + pos._2))
    val newHeight = math.max(chamber.currentHeight, finalRock.map(_._2 + 1).max)

    Chamber(
      rocks = chamber.rocks ++ finalRock,
      currentHeight = newHeight,
      rockIndex = chamber.rockIndex + 1,
      jetIndex = currentJetIndex
    )
  end dropRock

  def findCycle(input: String): (Map[State, CycleInfo], Chamber, Long) =
    var chamber = Chamber()
    val seen = scala.collection.mutable.Map[State, CycleInfo]()
    var rockCount = 0L

    while true do
      val state = State(
        chamber.rockIndex % Rock.values.length,
        chamber.jetIndex % input.length,
        getProfile(chamber)
      )

      if seen.contains(state) then
        return (seen.toMap, chamber, rockCount)

      seen(state) = CycleInfo(rockCount, chamber.currentHeight)
      chamber = dropRock(chamber, input)
      rockCount += 1

    throw new RuntimeException("No cycle found")
  end findCycle

  def solvePart1(input: Seq[String]): Int =
    val jets = input.head
    val finalChamber = (1 to 2022).foldLeft(Chamber())((chamber, _) =>
      dropRock(chamber, jets)
    )
    finalChamber.currentHeight

  def solvePart2(input: Seq[String]): Long =
    val target = 1000000000000L
    val jets = input.head

    val (seen, chamber, currentRocks) = findCycle(jets)
    val state = State(
      chamber.rockIndex % Rock.values.length,
      chamber.jetIndex % jets.length,
      getProfile(chamber)
    )

    val cycleStart = seen(state)
    val cycleLength = currentRocks - cycleStart.rockCount
    val heightGain = chamber.currentHeight - cycleStart.height

    val remainingCycles = (target - cycleStart.rockCount) / cycleLength
    val remainingRocks = (target - cycleStart.rockCount) % cycleLength

    var finalChamber = chamber
    for _ <- 0L until remainingRocks do
      finalChamber = dropRock(finalChamber, jets)

    val baseHeight = cycleStart.height
    val cycleHeight = remainingCycles * heightGain
    val remainingHeight = finalChamber.currentHeight - chamber.currentHeight

    baseHeight + cycleHeight + remainingHeight
end Day17
