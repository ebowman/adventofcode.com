package y2015

import org.scalatest.funsuite._

class Day03Test extends AnyFunSuite {

  import y2015.Day03._

  test("known inputs") {
    assert(visits(">") === 2)
    assert(visits("^>v<") === 4)
    assert(visits("^v^v^v^v^v") === 2)
    assert(visitsRobo("^v") === 3)
    assert(visitsRobo("^>v<") === 3)
    assert(visitsRobo("^v^v^v^v^v") === 11)
  }
}
