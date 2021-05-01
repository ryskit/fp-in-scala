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
}
