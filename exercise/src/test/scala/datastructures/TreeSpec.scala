package datastructures

import org.scalatest.funspec.AnyFunSpec

class TreeSpec extends AnyFunSpec {
  describe("EXERCISE3.25") {
    it("should be true") {
      val b = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))
      assert(Tree.size(b) == 7)
    }
  }

  describe("EXERCISE3.26") {
    it("should be true") {
      val b = Branch(
        Branch(Leaf(1), Leaf(3)),
        Branch(Branch(Leaf(1000), Leaf(-1)), Leaf(100))
      )
      assert(Tree.max(b) == 1000)
    }
  }

  describe("EXERCISE3.27") {
    it("should be true") {
      val b = Branch(
        Branch(Leaf(1), Leaf(3)),
        Branch(Branch(Leaf(1000), Branch(Leaf(1), Leaf(5))), Leaf(100))
      )
      assert(Tree.depth(b) == 5)
    }
  }

  describe("EXERCISE3.28") {
    it("should be true") {
      val b = Branch(
        Branch(Leaf(1), Leaf(3)),
        Branch(Branch(Leaf(1000), Branch(Leaf(1), Leaf(5))), Leaf(100))
      )
      assert(
        Tree.map(b)(x => x.toString) == Branch(
          Branch(Leaf("1"), Leaf("3")),
          Branch(
            Branch(Leaf("1000"), Branch(Leaf("1"), Leaf("5"))),
            Leaf("100")
          )
        )
      )
    }
  }

  describe("EXERCISE3.29") {
    describe("sizeUseFold") {
      it("should be true") {
        val b = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
        assert(Tree.sizeUseFold(b) == 7)
      }
    }

    describe("maximumUseFold") {
      it("should be true") {
        val b = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
        assert(Tree.maximumUseFold(b) == 4)
      }
    }

    describe("depthUseFold") {
      it("should be true") {
        val b = Branch(
          Branch(Leaf(1), Leaf(3)),
          Branch(Branch(Leaf(1000), Branch(Leaf(1), Leaf(5))), Leaf(100))
        )
        assert(Tree.depthUseFold(b) == 5)
      }
    }
  }
}
