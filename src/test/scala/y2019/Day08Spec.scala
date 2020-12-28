package y2019

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day08Spec extends AnyFlatSpec with Matchers with Day08 {

  lazy val input: String = Loader(this, "day08.txt").head

  it should "solve part 1 tests" in {
    part1(2, 3, "123456789012") shouldBe 1
  }

  it should "solve part 1" in {
    part1(6, 25, input) shouldBe 2016
  }

  it should "solve part 2 test" in {
    part2(2, 2, "0222112222120000") shouldBe " #\n# "
  }

  it should "solve part 2" in {
    part2(6, 25, input) shouldBe "" +
      "#  # ####  ##  #### #  # \n" +
      "#  #    # #  #    # #  # \n" +
      "####   #  #      #  #  # \n" +
      "#  #  #   #     #   #  # \n" +
      "#  # #    #  # #    #  # \n" +
      "#  # ####  ##  ####  ##  "
  }
}
