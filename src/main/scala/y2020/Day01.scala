package y2020

import util.Loader

object Day01 {

  /** Load the custom inputs (from src/main/resources/aoc02/input.txt) into a Set[Int] */
  lazy val inputs = Loader(this, "day01.txt").map(_.toInt).toSet
  lazy val sum2 = s2(inputs)
  lazy val sum3 = s3(inputs)

  /**
   * Given a Set[Int], assumes there are two values within it that sum to 2020. Finds them and returns their product.
   * If the set does not include two values that add up to 2020, will blow come kind of runtime exception as it tries
   * to pull the head off an empty collection.
   */
  def s2(inputs: Set[Int]): Int = {
    inputs.flatMap { a =>
      inputs.find(_ == 2020 - a).map(_ * a)
    }.head
  }

  /** Similar to s2, but this assumes there are 3 values which together add up to 2020, and returns their product. */
  def s3(inputs: Set[Int]): Int = {
    inputs.flatMap { a =>
      inputs.withFilter(_ != a).flatMap { b =>
        inputs.find(_ == 2020 - a - b).map(_ * b * a)
      }
    }.head
  }
}
