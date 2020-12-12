package y2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import util.Loader

class Day12Spec extends AnyFlatSpec with Matchers with Day12 {
  "Basic tests" should "pass" in {
    countNums("[1,2,3]") shouldBe 6
    countNums("{\"a\":2,\"b\":4}") shouldBe 6
    countNums("{\"a\":[-1,1]}") shouldBe 0
    countNums("[-1,{\"a\":1}]") shouldBe 0
    countNums("[]") shouldBe 0
  }
  it should "pass the test input" in {
    countNums(Loader(this,"day12.txt").head) shouldBe 191164
  }
  "test 2" should "pass simple test cases" in {
    count("[1,2,3]") shouldBe 6
    count("[1,{\"c\":\"red\",\"b\":2},3]") shouldBe 4
    count("{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}") shouldBe 0
    count("[1,\"red\",5]") shouldBe 6
  }
  it should "pass the final test" in {
    count(Loader(this,"day12.txt").head) shouldBe 87842
  }
}
