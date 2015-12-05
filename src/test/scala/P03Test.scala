import org.scalatest.FunSuite

class P03Test extends FunSuite {
  import P03._

  test("known inputs") {
    assert(visits(">") === 2)
    assert(visits("^>v<") === 4)
    assert(visits("^v^v^v^v^v") === 2)
    assert(visitsRobo("^v") === 3)
    assert(visitsRobo("^>v<") === 3)
    assert(visitsRobo("^v^v^v^v^v") === 11)
  }
}
