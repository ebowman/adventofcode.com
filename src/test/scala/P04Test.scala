import org.scalatest.FunSuite

class P04Test extends FunSuite {
  import P04._

  test("known inputs") {
    assert(solve("abcdef") === 609043)
  }
}
