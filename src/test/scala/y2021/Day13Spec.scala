package y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day13Spec extends AnyFlatSpec with Matchers with Day13 {
  lazy val testInput = Loader(this, "day13.test.txt").toSeq
  lazy val input = Loader(this, "day13.txt").toSeq

  "Day13" should "pass the part 1 tests" in {
    solve1(testInput) shouldBe 17
  }

  it should "pass part 1" in {
    solve1(input) shouldBe 759
  }

  it should "pass part 2 test" in {
    solve2(testInput) shouldBe
      """|#####
         |#   #
         |#   #
         |#   #
         |#####""".stripMargin
  }

  it should "pass part 2" in {
    solve2(input) shouldBe
      "#  # ####  ##  ###  #### #  # ###  ### \n" +
      "#  # #    #  # #  #    # # #  #  # #  #\n" +
      "#### ###  #    #  #   #  ##   #  # #  #\n" +
      "#  # #    #    ###   #   # #  ###  ### \n" +
      "#  # #    #  # # #  #    # #  #    # # \n" +
      "#  # ####  ##  #  # #### #  # #    #  #"
  }
}
