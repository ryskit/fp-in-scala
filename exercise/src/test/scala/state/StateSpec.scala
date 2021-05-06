package state

import org.scalatest.funspec.AnyFunSpec
import RNG._

class StateSpec extends AnyFunSpec {
  describe("EXERCISE6.4") {
    it("should be true") {
      val srng = SimpleRNG(1000L)
      val (li3, rng) = ints(3)(srng)
      println(li3)
      assert(li3.length == 3)
    }
  }

  describe("EXERCISE6.5") {
    it("should be true") {
      val srng = SimpleRNG(1000L)
      val (d1, r1) = double(srng)
      val (d2, r2) = doubleUseMap(srng)
      assert(d2 == d1)
    }
  }
}
