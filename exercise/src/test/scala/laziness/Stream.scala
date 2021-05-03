package laziness

import org.scalatest.funspec.AnyFunSpec

class Stream extends AnyFunSpec {
  describe("EXERCISE5.1") {
    it("should be true") {
      val s = Stream.apply(1, 2, 3, 4, 5)
      assert(s.toList == List(1, 2, 3, 4, 5))
    }
  }
}
