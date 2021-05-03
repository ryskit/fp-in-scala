package errorhandling

import org.scalatest.funspec.AnyFunSpec

class OptionSpec extends AnyFunSpec {
  describe("EXERCISE4.1") {
    describe("map") {
      it("should be true") {
        assert(Some(1).map(_ * 2) == Some(2))
      }

      it("should be None") {
        assert(None.map(nothing => 2) == None)
      }
    }

    describe("flatMap") {
      it("should be true") {
        assert(Some(2).flatMap(n => Some(n * 2)) == Some(4))
      }
      it("should be None") {
        assert(None.flatMap(n => None) == None)
      }
    }

    describe("getOrElse") {
      it("should be true") {
        assert(Some(2).getOrElse(10) == 2)
      }
      it("should be None") {
        assert(None.getOrElse(10) == 10)
      }
    }

    describe("orElse") {
      it("should be true") {
        assert(Some(2).orElse(Some(10)) == Some(2))
      }
      it("should be None") {
        assert(None.orElse(Some(10)) == Some(10))
      }
    }

    describe("filter") {
      it("should be true") {
        assert(Some(2).filter(x => x > 1) == Some(2))
      }
      it("should be None") {
        assert(Some(2).filter(x => x > 2) == None)
      }
    }
  }

  describe("EXERCISE4.3") {
    it("should be true") {
      assert(Option.map2(Some(2), Some(3))(_ + _) == Some(5))
    }
  }

  describe("EXERCISE4.4") {
    it("should be true") {
      val l = List(Some(1), Some(2), Some(3))
      assert(Option.sequence(l) == Some(List(1, 2, 3)))
    }
  }

  describe("EXERCISE4.5") {
    it("should be true") {
      val l = List(1, 2, 3)
      assert(Option.traverse(l) == Some(List(1, 2, 3)))
    }
  }
}
