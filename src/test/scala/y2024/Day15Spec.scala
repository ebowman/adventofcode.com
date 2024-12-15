package y2024

class Day15Spec extends util.DaySpec(new Day15):

  it should "solve part 1 test" in:
    solution.solvePart1(testInput) shouldBe 2028
    solution.solvePart1(testInput2) shouldBe 10092

  it should "solve part 1" in:
    solution.solvePart1(input) shouldBe 1_511_865

  it should "solve part 2 test" in:
    solution.solvePart2(testInput2) shouldBe 9021

  it should "solve part 2" in:
    solution.solvePart2(input) shouldBe 1519991
