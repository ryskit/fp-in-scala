package laziness

import org.scalatest.funspec.AnyFunSpec

import Stream._

class StreamSpec extends AnyFunSpec {
  describe("EXERCISE5.1") {
    it("should be true") {
      val s = Stream.apply(1, 2, 3, 4, 5)
      assert(s.toList == List(1, 2, 3, 4, 5))
    }
  }

  describe("EXERCISE5.2") {
    describe("take") {
      it("should be true") {
        val s = Stream.apply(1, 2, 3, 4, 5)
        assert(s.take(2).toList == Stream.apply(1, 2).toList)
      }
    }

    describe("drop") {
      it("should be true") {
        val s = Stream.apply(1, 2, 3, 4, 5)
        assert(s.drop(2).toList == Stream.apply(3, 4, 5).toList)
      }
    }
  }

  describe("EXERCISE5.3") {
    it("should be true") {
      val s = Stream.apply(1, 2, 3, 4, 5)
      assert(s.takeWhile(_ < 3) == Stream.apply(1, 2))
    }
  }

  describe("EXERCISE5.4") {
    it("should be true") {
      assert(Stream.apply(2, 4, 6, 8, 10).forAll(_ % 2 == 0))
    }

    it("should be false") {
      assert(!Stream.apply(1, 2, 3, 4, 5).forAll(_ % 2 == 0))
    }
  }

  describe("EXERCISE5.5") {
    it("should be true") {
      val s = Stream.apply(1, 2, 3, 4, 5)
      assert(s.takeWhileUseFoldRight(_ < 3).toList == Stream.apply(1, 2).toList)
    }
  }

  describe("EXERCISE5.6") {
    it("should be true") {
      val s = Stream.apply(1, 2, 3, 4, 5)
      assert(s.headOptionUseFoldRight.contains(1))
    }
  }

  describe("EXERCISE5.7") {
    describe("map") {
      it("should be true") {
        val s = Stream.apply(1, 2, 3, 4, 5)
        assert(s.map(_ * 2).toList == Stream(2, 4, 6, 8, 10).toList)
      }
    }

    describe("filter") {
      it("should be true") {
        val s = Stream.apply(1, 2, 3, 4, 5)
        assert(s.filter(_ % 2 == 0).toList == Stream(2, 4).toList)
      }
    }

    describe("append") {
      it("should be true") {
        val s1 = Stream.apply(1, 2, 3, 4, 5)
        val s2 = Stream.apply(6, 7, 8, 9, 10)
        assert(
          s1.append(s2).toList == Stream(1, 2, 3, 4, 5, 6, 7, 8, 9, 10).toList
        )
      }
    }

    describe("flatMap") {
      it("should be true") {
        val s = Stream.apply(1, 2, 3, 4, 5)
        assert(
          s.flatMap(x => cons(x * 2, empty))
            .toList == Stream(2, 4, 6, 8, 10).toList
        )
      }
    }
  }

  describe("EXERCISE5.12") {
    describe("onesUseUnfold") {
      it("should be true") {
        println(onesUseUnfold().take(5).toList == List(1, 1, 1, 1, 1))
      }
    }

    describe("constantUseUnfold") {
      it("should be true") {
        println(constantUseUnfold(2).take(5).toList == List(2, 2, 2, 2, 2))
      }
    }

    describe("fromUseUnfold") {
      it("should be true") {
        println(fromUseUnfold(1).take(5).toList == List(1, 2, 3, 4, 5))
      }
    }

    describe("fibsUseUnfold") {
      it("should be true") {
        println(fibsUseUnfold().take(5).toList == List(0, 1, 1, 2, 3))
      }
    }
  }

}
