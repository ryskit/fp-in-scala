package gettingstarted

import org.scalatest.funspec.AnyFunSpec

class GettingStartedSpec extends AnyFunSpec {

  describe("EXERCISE2.2") {
    val ordered = (x: Int, y: Int) => x < y
    it("should be true") {
      assert(GettingStarted.isSorted(Array(1, 2, 3, 4, 5), ordered))
    }

    it("should be false") {
      assert(!GettingStarted.isSorted(Array(1, 3, 2, 5, 4), ordered))
    }
  }

  describe("EXERCISE2.3") {
    it("should be true") {
      val uncurry = GettingStarted.uncurry((x: Int) => (y: Int) => x * y)
      val curry = GettingStarted.curry(uncurry)
      assert(curry(2)(3) == 6)
    }
  }

  describe("EXERCISE2.4") {
    it("should be true") {
      val curry = GettingStarted.curry((x: Int, y: Int) => x * y)
      val uncurry = GettingStarted.uncurry(curry)
      assert(uncurry(2, 3) == 6)
    }
  }

  describe("EXERCISE2.5") {
    it("should be true") {
      val f = (b: Int) => b * 2
      val g = (a: Int) => a + 2
      val compose = GettingStarted.compose(f, g)
      assert(compose(5) == 14)
    }
  }
}
