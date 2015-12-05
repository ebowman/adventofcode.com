import org.scalatest.FunSuite

class P02Test extends FunSuite {
  import P02._

  test("known inputs") {
    assert(total("2x3x4") === 58)
    assert(total("1x1x10") === 43)
    assert(ribbon("2x3x4") === 34)
    assert(ribbon("1x1x10") === 14)
  }

}
