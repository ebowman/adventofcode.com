package y2024

class Day05Spec extends util.DaySpec(new Day05):

  it should "solve part 1 test" in:
    solution.solvePart1(testInput) shouldBe 143

  it should "solve part 1" in:
    solution.solvePart1(input) shouldBe 6384

  it should "solve part 2 test" in:
    solution.solvePart2(testInput) shouldBe 123

  it should "solve part 2" in:
    solution.solvePart2(input) shouldBe 5353
