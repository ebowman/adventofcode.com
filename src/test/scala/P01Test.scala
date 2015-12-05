import org.scalatest.FunSuite

class P01Test extends FunSuite {
  test("sample inputs") {
    assert(P01.compute("(())") === 0)
    assert(P01.compute("()()") === 0)
    assert(P01.compute("(((") === 3)
    assert(P01.compute("(()(()(") === 3)
    assert(P01.compute("))(((((") === 3)
    assert(P01.compute("())") === -1)
    assert(P01.compute("))(") === -1)
    assert(P01.compute(")))") === -3)
    assert(P01.compute(")())())") === -3)
  }
}
