package y2015

import org.scalatest.funsuite._

class Day04Test extends AnyFunSuite {
  import y2015.Day04._

  test("known inputs") {
    assert(solve("abcdef") === 609043)
  }
}
