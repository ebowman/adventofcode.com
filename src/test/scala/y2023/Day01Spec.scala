
package y2023

class Day01Spec extends util.DaySpec(new Day01):
  it should "solve part 1 test" in:
    solution.solvePart1(testInput) shouldBe 142

  it should "solve part 1" in:
    solution.solvePart1(input) shouldBe 55538

  it should "solve part 2 test" in:
    solution.solvePart2(testInput2) shouldBe 281

  it should "solve part 2" in:
    solution.solvePart2(input) shouldBe 54875
