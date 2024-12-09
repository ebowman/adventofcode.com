package y2024

class Day09Spec extends util.DaySpec(new Day09):

  it should "solve part 1 test" in:
    solution.solvePart1(testInput) shouldBe 1928

  it should "solve part 1" in:
    solution.solvePart1(input) shouldBe 6_201_130_364_722L

  it should "solve part 2 test" in:
    solution.solvePart2(testInput) shouldBe 2858

  it should "solve part 2" in:
    solution.solvePart2(input) shouldBe 6_221_662_795_602L
