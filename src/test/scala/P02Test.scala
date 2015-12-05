import org.scalatest.FunSuite

class P02Test extends FunSuite {
  import P02._

  test("known inputs") {
    assert(dim("2x3x4") === 58)
    assert(dim("1x1x10") === 43)
  }

}
