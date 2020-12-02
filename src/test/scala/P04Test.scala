import org.scalatest.funsuite._

class P04Test extends AnyFunSuite {
  import P04._

  test("known inputs") {
    assert(solve("abcdef") === 609043)
  }
}
