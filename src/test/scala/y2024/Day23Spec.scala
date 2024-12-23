package y2024

class Day23Spec extends util.DaySpec(new Day23):

  it should "solve part 1 test" in:
    solution.solvePart1(testInput) shouldBe 7

  it should "solve part 1" in:
    solution.solvePart1(input) shouldBe 1253

  it should "solve part 2 test" in:
    solution.solvePart2(testInput) shouldBe "co,de,ka,ta"

  it should "solve part 2" in:
    solution.solvePart2(input) shouldBe "ag,bt,cq,da,hp,hs,mi,pa,qd,qe,qi,ri,uq"
