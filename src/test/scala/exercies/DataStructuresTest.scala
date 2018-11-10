package exercies

import exercies.DataStructures._
import org.scalatest.FunSpec

import scala.annotation.tailrec

class DataStructuresTest extends FunSpec {

  describe("IList") {
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

    describe("take") {
      it("works on empty list") {
        assert(IList.take(INil, 10) == INil)
      }

      it("takes first N elements") {
        assert(IList.take(IList(1, 2, 3, 4), 3) == IList(1, 2, 3))
      }

      it("can not take more than there is") {
        assert(IList.take(IList(1, 2, 3, 4), 5) == IList(1, 2, 3, 4))
      }

      it("can also take nothing") {
        assert(IList.take(IList(1, 2, 3, 4), 0) == INil)
        assert(IList.take(IList(1, 2, 3, 4), -10) == INil)
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

    describe("productFold") {
      it("works on empty list") {
        assert(IList.productFoldRight(INil) == 1)
      }

      it("multiplies of elements of the list") {
        assert(IList.productFoldRight(IList(1, 2, 3, 4)) == 24)
      }

      it("handles zero") {
        assert(IList.productFoldRight(IList(1, 2, 0, 4)) == 0)
      }
    }

    describe("length") {
      it("works on empty list") {
        assert(IList.length(INil) == 0)
      }

      it("counts the number of elements in non-empty list") {
        assert(IList.length(IList(1, 2, 3)) == 3)
      }
    }

    describe("foldLeft") {
      def sum(as: IList[Int]): Int =
        IList.foldLeft(as, 0)(_ + _)

      it("works on empty list") {
        assert(sum(INil) == 0)
      }

      it("works on even number of elements") {
        assert(sum(IList(1, 2, 3, 4)) == 10)
      }

      it("works on odd number of elements") {
        assert(sum(IList(1, 2, 3)) == 6)
      }

      // What's the reason it doesn't?
      //    it("works for very large lists") {
      //      val bigNumbers = 1 to 10000
      //      assert(sum(IList(bigNumbers: _*)) == 100)
      //    }
    }

    describe("sumFoldLeft") {
      it("works for empty list") {
        assert(IList.sumFoldLeft(INil) == 0)
      }

      it("sums up integers") {
        assert(IList.sumFoldLeft(IList(1, 2, 3, 4)) == 10)
      }
    }

    describe("productFoldRight") {
      it("works on empty list") {
        assert(IList.productFoldRight(INil) == 1)
      }

      it("multiplies of elements of the list") {
        assert(IList.productFoldRight(IList(1, 2, 3, 4)) == 24)
      }

      it("handles zero") {
        assert(IList.productFoldRight(IList(1, 2, 0, 4)) == 0)
      }
    }

    describe("reverse") {
      it("works on empty list") {
        assert(IList.reverse(INil) == INil)
      }

      it("works on single element lists") {
        assert(IList.reverse(IList(1)) == IList(1))
      }

      it("reverses list") {
        assert(IList.reverse(IList(1, 2, 3)) == IList(3, 2, 1))
      }
    }

    describe("append") {
      it("works on empty list") {
        assert(IList.append(IList.empty[Int], 1) == IList(1))
      }

      it("appens element to a list") {
        assert(IList.append(IList(1, 2, 3), 4) == IList(1, 2, 3, 4))
      }
    }

    describe("union") {
      it("works with empty lists") {
        assert(IList.union(IList.empty[Int], IList.empty[Int]) == IList.empty[Int])
      }

      it("works with empty list on the left") {
        assert(IList.union(IList.empty[Int], IList(1, 2, 3)) == IList(1, 2, 3))
      }

      it("works with empty list on the right") {
        assert(IList.union(IList(1, 2, 3), IList.empty[Int]) == IList(1, 2, 3))
      }

      it("concatenates lists") {
        assert(IList.union(IList(1, 2, 3), IList(4, 5, 6)) == IList(1, 2, 3, 4, 5, 6))
      }
    }

    describe("plusOne") {
      it("works with empty lists") {
        assert(IList.plusOne(INil) == INil)
      }

      it("adds one") {
        assert(IList.plusOne(IList(1, 2, 3)) == IList(2, 3, 4))
      }
    }

    describe("doubleToString") {
      it("works with empty lists") {
        assert(IList.doubleToString(INil) == INil)
      }

      it("adds one") {
        assert(IList.doubleToString(IList(1, 2, 3)) == IList("1", "2", "3"))
      }
    }

    describe("map") {
      it("works on empty lists") {
        assert(IList.map(IList.empty[Int])(_ + 1) == IList.empty[Int])
      }

      it("maps a function over a list") {
        assert(IList.map(IList(1, 2, 3))(_ + 1) == IList(2, 3, 4))
      }
    }

    describe("filter") {
      it("works on empty list") {
        assert(IList.filter(IList.empty[Int])(_ % 2 == 0) == IList.empty[Int])
      }

      it("filters elements which do not match predicate") {
        assert(IList.filter(IList(1, 2, 3, 4, 5, 6))(_ % 2 == 0) == IList(2, 4, 6))
      }
    }

    describe("flatMap") {
      it("works on empty list") {
        assert(IList.flatMap(IList.empty[Int])(_ => IList(1, 2, 3)) == IList.empty[Int])
      }

      it("maps and flattens") {
        val multiplier123 = (x: Int) => IList.map(IList(1, 2, 3))(_ * x)

        assert(IList.flatMap(IList(1, 2, 3))(multiplier123) == IList(1, 2, 3, 2, 4, 6, 3, 6, 9))
      }
    }

    describe("filterFlatMap") {
      it("works on empty list") {
        assert(IList.filterFlatMap(IList.empty[Int])(_ % 2 == 0) == IList.empty[Int])
      }

      it("filters elements which do not match predicate") {
        assert(IList.filterFlatMap(IList(1, 2, 3, 4, 5, 6))(_ % 2 == 0) == IList(2, 4, 6))
      }
    }

    describe("sumElements") {
      it("works on empty lists") {
        assert(IList.sumElements(IList.empty[Int], IList.empty[Int]) == IList.empty[Int])
      }

      it("sums corresponding pairs") {
        assert(IList.sumElements(IList(1, 2, 3), IList(4, 5, 6)) == IList(5, 7, 9))
      }

      it("dies if lengths don't match") {
        assertThrows[RuntimeException] {
          IList.sumElements(IList(1, 2, 3), IList(4, 5))
        }
      }
    }

    describe("zipWith") {
      it("works on empty lists") {
        assert(IList.zipWith(IList.empty[Int], IList.empty[Int])((_, _)) == IList.empty[(Int, Int)])
      }

      it("sums corresponding pairs") {
        assert(IList.zipWith(IList(1, 2, 3), IList(4, 5, 6))((_, _)) == IList((1, 4), (2, 5), (3, 6)))
      }

      it("dies if lengths don't match") {
        assertThrows[RuntimeException] {
          IList.zipWith(IList(1, 2, 3), IList(4, 5))((_, _))
        }
      }
    }

    describe("hasSubsequence") {
      it("works on empty lists") {
        assert(IList.hasSubsequence(INil, INil))
      }

      it("nothing is subset of anything") {
        assert(IList.hasSubsequence(IList(1, 2, 3), INil))
      }

      it("nothing is subset of empty set") {
        assert(!IList.hasSubsequence(INil, IList(1, 2, 3)))
      }

      it("bigger subset can not be subsequence of smaller one") {
        assert(!IList.hasSubsequence(IList(1, 2, 3), IList(1, 2, 3, 4)))
      }

      it("detect subsets") {
        assert(IList.hasSubsequence(IList(1, 2, 3, 4), IList(2, 3)))
      }

      it("detects subsets on left boundary") {
        assert(IList.hasSubsequence(IList(1, 2, 3, 4), IList(1, 2)))
      }

      it("detects subsets on right boundary") {
        assert(IList.hasSubsequence(IList(1, 2, 3, 4), IList(3, 4)))
      }

      it("correctly detects absence on subset") {
        assert(!IList.hasSubsequence(IList(1, 2, 3, 4), IList(2, 5)))
      }
    }
  }

  describe("Tree") {
    describe("size") {
      it("counts branches and leafs") {
        assert(Tree.size(Branch(Branch(Leaf(0), Leaf(1)), EmptyLeaf)) == 4)
      }
    }

    describe("maximum") {
      it("finds maximum element of the tree") {
        assert(Tree.maxiumum(Branch(Branch(Leaf(0), EmptyLeaf), Leaf(2))) == 2)
      }
    }

    describe("depth") {
      it("gets maximum depth of the tree") {
        assert(Tree.depth(Branch(Branch(Leaf(0), EmptyLeaf), Leaf(2))) == 3)
      }
    }

    describe("map") {
      it("transforms all leaf values with f") {
        val in = Branch(Branch(Leaf(0), EmptyLeaf), Leaf(2))
        val out = Branch(Branch(Leaf("0"), EmptyLeaf), Leaf("2"))
        assert(Tree.map(in)(_.toString) == out)
      }
    }

    describe("sizeFold") {
      it("counts branches and leafs") {
        assert(Tree.sizeFold(Branch(Branch(Leaf(0), Leaf(1)), EmptyLeaf)) == 4)
      }
    }

    describe("maximumFold") {
      it("finds maximum element of the tree") {
        assert(Tree.maxiumumFold(Branch(Branch(Leaf(0), EmptyLeaf), Leaf(2))) == 2)
      }
    }

    describe("depthFold") {
      it("gets maximum depth of the tree") {
        assert(Tree.depthFold(Branch(Branch(Leaf(0), EmptyLeaf), Leaf(2))) == 3)
      }
    }

    describe("mapFold") {
      it("transforms all leaf values with f") {
        val in = Branch(Branch(Leaf(0), EmptyLeaf), Leaf(2))
        val out = Branch(Branch(Leaf("0"), EmptyLeaf), Leaf("2"))
        assert(Tree.mapFold(in)(_.toString) == out)
      }
    }
  }

}
