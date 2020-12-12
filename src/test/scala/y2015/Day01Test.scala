package y2015

import org.scalatest.funsuite.AnyFunSuite

class Day01Test extends AnyFunSuite {
  test("sample inputs") {
    assert(Day01.compute("(())") === 0)
    assert(Day01.compute("()()") === 0)
    assert(Day01.compute("(((") === 3)
    assert(Day01.compute("(()(()(") === 3)
    assert(Day01.compute("))(((((") === 3)
    assert(Day01.compute("())") === -1)
    assert(Day01.compute("))(") === -1)
    assert(Day01.compute(")))") === -3)
    assert(Day01.compute(")())())") === -3)
  }
}
