package errorhandling

import org.scalatest.funspec.AnyFunSpec

class EitherSpec extends AnyFunSpec {
  describe("EXERCISE4.6") {
    describe("map") {
      it("should be true") {
        assert(Right(2).map(_ * 2) == Right(4))
      }
    }

    describe("flatMap") {
      it("should be true") {
        assert(Right(2).flatMap(x => Right(x.toString)) == Right("2"))
      }
    }

    describe("orElse") {
      it("should be true") {
        assert(Right(2).orElse(Right("error")) == Right(2))
        assert(Left(2).orElse(Right("error")) == Right("error"))
      }
    }

    describe("map2") {
      it("should be true") {
        assert(Right(2).map2(Right(5))((a, b) => a * b) == Right(10))
      }
    }
  }
}
