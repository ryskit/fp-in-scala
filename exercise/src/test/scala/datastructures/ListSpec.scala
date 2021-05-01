package datastructures

import org.scalatest.funspec.AnyFunSpec

class ListSpec extends AnyFunSpec {
  describe("EXERCISE3.2") {
    it("should be true") {
      val l = List(1, 2, 3, 4, 5)
      assert(List.tail(l) == List(2, 3, 4, 5))
    }
  }

  describe("EXERCISE3.3") {
    it("should be true") {
      val l = List(1, 2, 3, 4, 5)
      assert(List.setHead(l, 10) == List(10, 2, 3, 4, 5))
    }
  }

  describe("EXERCISE3.4") {
    it("should be true") {
      val l = List(1, 2, 3, 4, 5)
      assert(List.drop(l, 2) == List(3, 4, 5))
    }
  }

  describe("EXERCISE3.5") {
    it("should be true") {
      val l = List(1, 2, 3, 4, 5)
      assert(List.dropWhile(l, (x: Int) => x < 4) == List(4, 5))
    }

    it("should be true if list is empty") {
      val l = Nil
      assert(List.dropWhile(l, (x: Int) => x <= 3) == Nil)
    }
  }

  describe("EXERCISE3.6") {
    it("should be true") {
      val l = List(1, 2, 3, 4, 5)
      assert(List.init(l) == List(1, 2, 3, 4))
      println(List.length(l))
    }

    it("should be true if list is empty") {
      val l = Nil
      assert(List.init(l) == Nil)
    }
  }

  describe("EXERCISE3.9") {
    it("should be true") {
      val l = List(1, 2, 3, 4, 5)
      assert(List.length(l) == 5)
    }
  }

  describe("EXERCISE3.10") {
    it("should be true") {
      val l = List(1, 2, 3, 4, 5)
      assert(List.foldLeft(l, 0)(_ + _) == 15)
    }
  }

  describe("EXERCISE3.11") {
    describe("sumLF") {
      it("should be true") {
        val l = List(1, 2, 3, 4, 5)
        assert(List.sumLF(l) == 15)
      }
    }
    describe("productLF") {
      it("should be true") {
        val l = List(1.0, 2.0, 3.0, 4.0, 5.0)
        assert(List.productLF(l) == 120.0)
      }
    }
    describe("lengthLF") {
      it("should be true") {
        val l = List(1, 2, 3, 4, 5)
        assert(List.lengthLF(l) == 5)
      }
    }
  }

  describe("EXERCISE3.12") {
    it("should be true") {
      val l = List(1, 2, 3, 4, 5)
      assert(List.reverse(l) == List(5, 4, 3, 2, 1))
    }
  }

  describe("EXERCISE3.14") {
    val l1 = List(1, 2, 3, 4, 5)
    val l2 = List(6, 7, 8, 9, 10)
    val exp = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    describe("appendLF") {
      it("should be true") {
        assert(List.appendLF(l1, l2) == exp)
      }
    }

    describe("appendRF") {
      it("should be true") {
        assert(List.appendRF(l1, l2) == exp)
      }
    }
  }
}
