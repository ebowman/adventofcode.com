package y2015

import org.scalatest.funsuite._

class Day05Test extends AnyFunSuite {

  import y2015.Day05._

  test("known inputs") {
    assert(isNice("ugknbfddgicrmopn"))
    assert(isNice("aaa"))
    assert(!isNice("jchzalrnumimnmhp"))
    assert(!isNice("haegwjzuvuyypxyu"))
    assert(!isNice("dvszwmarrgswjxmb"))
  }

  test("part 2 known inputs") {
    assert(isNice2("qjhvhtzxzqqjkmpb"))
    assert(isNice2("xxyxx"))
    assert(isNice2("abaaaab"))
    assert(!isNice2("aaa"))
    assert(!isNice2("uurcxstgmygtbstg"))
    assert(!isNice2("ieodomkazucvgmuy"))
  }
  test("correct solutions") {
    assert(inputs.count(isNice) == 238)
    assert(inputs.count(isNice2) == 69)
  }
}
