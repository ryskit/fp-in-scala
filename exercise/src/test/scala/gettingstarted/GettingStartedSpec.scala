package gettingstarted

import org.scalatest.funspec.AnyFunSpec

class GettingStartedSpec extends AnyFunSpec {

  describe("EXERCISE2.2") {
    val ordered = (x: Int, y: Int) => x < y
    it("should be true") {
      GettingStarted.isSorted(Array(1, 2, 3, 4, 5), ordered)
    }

    it("should be false") {
      GettingStarted.isSorted(Array(1, 3, 2, 5, 4), ordered)
    }
  }
}
