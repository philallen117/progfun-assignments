package recfun

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PascalSuite extends FunSuite {
  import Main.pascal

  test("pascal: col=0,row=0") {
    assert(pascal(0,2) === 1)
  }

  test("pascal: col=0,row=1") {
    assert(pascal(0,2) === 1)
  }

  test("pascal: col=5,row=5") {
    assert(pascal(5,5) === 1)
  }

  test("pascal: col=1,row=2") {
      assert(pascal(1,2) === 2)
  }

  test("pascal: col=1,row=3") {
    assert(pascal(1,3) === 3)
  }

  test("pascal: col=2,row=3") {
    assert(pascal(2,3) === 3)
  }

  test("pascal: col=3,row=3") {
    assert(pascal(3,3) === 1)
  }

  test("pascal: col=3, row=2") {
    val thrown = intercept[IllegalArgumentException] {
      pascal(3,2)
    }
    assert(thrown.getMessage === "Column undefined for that row")
  }

  test("pascal: col=3, row=-1") {
    val thrown = intercept[IllegalArgumentException] {
      pascal(3,-1)
    }
    assert(thrown.getMessage === "Row must be non-negative")
  }

  test("pascal: col=-1, row=2") {
    val thrown = intercept[IllegalArgumentException] {
      pascal(-1,2)
    }
    assert(thrown.getMessage === "Column must be non-negative")
  }

}
