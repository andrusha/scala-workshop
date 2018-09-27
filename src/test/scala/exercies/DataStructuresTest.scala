package exercies

import exercies.DataStructures.{ICons, IList, INil}
import org.scalatest.FunSpec

import scala.annotation.tailrec

class DataStructuresTest extends FunSpec {

  describe("apply") {
    it("works with empty list") {
      assert(IList() == INil)
    }

    it("works on single-element lists") {
      assert(IList(1) == ICons(1, INil))
    }

    it("works for multiple elements") {
      assert(IList("a", "b", "c") == ICons("a", ICons("b", ICons("c", INil))))
    }
  }

  describe("sum") {
    it("works for empty list") {
      assert(IList.sum(INil) == 0)
    }

    it("sums up integers") {
      assert(IList.sum(IList(1, 2, 3, 4)) == 10)
    }
  }

  describe("product") {
    it("works on empty list") {
      assert(IList.product(INil) == 1)
    }

    it("multiplies of elements of the list") {
      assert(IList.product(IList(1, 2, 3, 4)) == 24)
    }

    it("handles zero") {
      assert(IList.product(IList(1, 2, 0, 4)) == 0)
    }
  }

  describe("tail") {
    it("works on empty list") {
      assert(IList.tail(INil) == INil)
    }

    /**
      * Because our data structure is defined as a case class
      * we get equality by-value for free and are able to just
      * check for equality
      */
    it("all the elements, but first") {
      assert(IList.tail(IList(1, 2, 3, 4)) == IList(2, 3, 4))
    }
  }

  describe("drop") {
    it("works on empty list") {
      assert(IList.drop(INil, 0) == INil)
    }

    it("dropping nothing is list itself") {
      assert(IList.drop(IList(1, 2, 3, 4), 0) == IList(1, 2, 3, 4))
    }

    it("drops n elements") {
      assert(IList.drop(IList(1, 2, 3, 4), 2) == IList(3, 4))
    }

    it("can drop more than the number of elements list containts") {
      assert(IList.drop(IList(1, 2, 3, 4), 5) == INil)
    }

    /**
      * Function is total if it works for any possible value of given type
      *
      * As negative values of n are valid Int, then we should be able to accept them also
      * It's possible to restrict the allowed values by either declaring new type
      * like PositiveInt or by using dependent types (which do not have official support in Scala yet)
      */
    it("works correctly if n is negative") {
      assert(IList.drop(IList(1, 2, 3, 4), -42) == IList(1, 2, 3, 4))
    }
  }

  describe("dropWhile") {
    /**
      * Underscore `_` can be used as a placeholder to denote
      * argument which is not used in the function
      *
      * The type of this placeholder argument has to be specified,
      * as compiler infers types for the function parameters as a whole
      */
    it("works on empty list") {
      assert(IList.dropWhile(INil, (_: Int) => true) == INil)
    }

    it("works with false predicate") {
      assert(IList.dropWhile(IList(1, 2, 3, 4), (_: Int) => false) == IList(1, 2, 3, 4))
    }

    it("works with true predicate") {
      assert(IList.dropWhile(IList(1, 2, 3, 4), (_: Int) => true) == INil)
    }

    it("drops while predicate is true") {
      assert(IList.dropWhile(IList(1, 2, 3, 4), (x: Int) => x <= 2) == IList(3, 4))
    }
  }

  describe("init") {
    it("works on empty list") {
      assert(IList.init(INil) == INil)
    }

    it("works for one-element lists") {
      assert(IList.init(IList(1)) == INil)
    }

    it("returns everything but the last element") {
      assert(IList.init(IList(1, 2, 3, 4)) == IList(1, 2, 3))
    }
  }
}
