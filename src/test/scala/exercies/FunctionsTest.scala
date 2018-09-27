package exercies

import org.scalatest._

class FunctionsTest extends FunSpec with Matchers {
  describe("fib") {
    it("maps initial sequence") {
      assert(Functions.fib(0) == 0)
      assert(Functions.fib(1) == 1)
    }

    it("computes first value correctly") {
      assert(Functions.fib(2) == 1)
      assert(Functions.fib(3) == 2)
      assert(Functions.fib(4) == 3)
      assert(Functions.fib(5) == 5)
    }

    it("works for middle values") {
      assert(Functions.fib(10) == 55)
    }

    /**
      * Inefficient implementation would be out of stack or
      * execution will take too long to complete
      */
    it("doesn't OOM on bigger values") {
      assert(Functions.fib(80) == 23416728348467685L)
    }
  }

  describe("isSorted") {
    /**
      * Notice that we have to specify type of the parameters of the anonymous comparator manually.
      *
      * We can help compiler to infer the type by currying the function:
      *   def isSorted[A](as: Array[A])(ordered: (A, A) => Boolean): Boolean
      *
      * This is also why stdlib functions are curried, e.g.:
      *   def foldLeft[B](z: B)(op: (B, A) => B): B
      */
    it("works for empty array") {
      assert(Functions.isSorted(Array(), (x: Int, y: Int) => x <= y))
    }

    it("works for one-element array") {
      assert(Functions.isSorted(Array(1), (x: Int, y: Int) => x <= y))
    }

    it("detects sorted array") {
      assert(Functions.isSorted(Array(1, 2, 3, 4, 5), (x: Int, y: Int) => x <= y))
    }

    it("detects unsorted array") {
      assert(!Functions.isSorted(Array(1, 2, 3, 5, 4), (x: Int, y: Int) => x <= y))
    }

    it("works for odd sized arrays") {
      assert(!Functions.isSorted(Array(1, 1, 2, 3, 5, 8, 7), (x: Int, y: Int) => x <= y))
    }

    it("works on complex types") {
      val as = Array(Array(1, 2), Array(3, 4, 5), Array(6, 7, 8))
      val lengthCmp = (x: Array[Int], y: Array[Int]) => x.length <= y.length

      assert(Functions.isSorted(as, lengthCmp))
    }
  }

  describe("curry & uncurry") {
    it("f(x, y) == f(x)(y)") {
      val f = (x: Int, y: Int) => x + y
      assert(Functions.curry(f)(1)(2) == f(1, 2))
    }

    it("f(x)(y) == f(x, y)") {
      val f = (x: Int) => (y: Int) => x + y
      assert(Functions.uncurry(f)(1,2) == f(1)(2))
    }

    it("uncurry * curry = id") {
      val f = (x: Int, y: Int) => x + y

      assert(Functions.uncurry(Functions.curry(f))(1, 2) == f(1, 2))
    }
  }

  describe("compose") {
    it("equal to application (f ∘ g)(x) == g(f(x))") {
      val f = (x: Int) => x + 2
      val g = (y: Int) => y * 2
      assert(Functions.compose(f, g)(9) == g(f(9)))
    }

    it("associative f ∘ (g ∘ h) = (f ∘ g) ∘ h") {
      val f = (x: Int) => x + 2
      val g = (y: Int) => y * 2
      val h = (z: Int) => z / 2

      val left = Functions.compose(f, Functions.compose(g, h))
      val right = Functions.compose(Functions.compose(f, g), h)

      assert(left(269) == right(269))
    }
  }
}
