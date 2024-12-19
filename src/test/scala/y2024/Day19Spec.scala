package y2024

class Day19Spec extends util.DaySpec(new Day19):

  it should "solve part 1 test" in:
    solution.solvePart1(testInput) shouldBe 6

  it should "solve part 1" in:
    solution.solvePart1(input) shouldBe 340

  it should "solve part 2 test" in:
    solution.solvePart2(testInput) shouldBe 16

  it should "solve part 2" in:
    solution.solvePart2(input) shouldBe 717561822679428L
