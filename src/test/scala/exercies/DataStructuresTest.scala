package exercies

import exercies.DataStructures._
import org.scalatest.FunSpec

import scala.annotation.tailrec

class DataStructuresTest extends FunSpec {

  describe("IList") {
    describe("apply") {
      it("works with empty list") {
        assert(List() == Nil)
      }

      it("works on single-element lists") {
        assert(List(1) == Cons(1, Nil))
      }

      it("works for multiple elements") {
        assert(List("a", "b", "c") == Cons("a", Cons("b", Cons("c", Nil))))
      }
    }

    describe("sum") {
      it("works for empty list") {
        assert(List.sum(Nil) == 0)
      }

      it("sums up integers") {
        assert(List.sum(List(1, 2, 3, 4)) == 10)
      }
    }

    describe("product") {
      it("works on empty list") {
        assert(List.product(Nil) == 1)
      }

      it("multiplies of elements of the list") {
        assert(List.product(List(1, 2, 3, 4)) == 24)
      }

      it("handles zero") {
        assert(List.product(List(1, 2, 0, 4)) == 0)
      }
    }

    describe("tail") {
      it("works on empty list") {
        assert(List.tail(Nil) == Nil)
      }

      /**
        * Because our data structure is defined as a case class
        * we get equality by-value for free and are able to just
        * check for equality
        */
      it("all the elements, but first") {
        assert(List.tail(List(1, 2, 3, 4)) == List(2, 3, 4))
      }
    }

    describe("drop") {
      it("works on empty list") {
        assert(List.drop(Nil, 0) == Nil)
      }

      it("dropping nothing is list itself") {
        assert(List.drop(List(1, 2, 3, 4), 0) == List(1, 2, 3, 4))
      }

      it("drops n elements") {
        assert(List.drop(List(1, 2, 3, 4), 2) == List(3, 4))
      }

      it("can drop more than the number of elements list containts") {
        assert(List.drop(List(1, 2, 3, 4), 5) == Nil)
      }

      /**
        * Function is total if it works for any possible value of given type
        *
        * As negative values of n are valid Int, then we should be able to accept them also
        * It's possible to restrict the allowed values by either declaring new type
        * like PositiveInt or by using dependent types (which do not have official support in Scala yet)
        */
      it("works correctly if n is negative") {
        assert(List.drop(List(1, 2, 3, 4), -42) == List(1, 2, 3, 4))
      }
    }

    describe("take") {
      it("works on empty list") {
        assert(List.take(Nil, 10) == Nil)
      }

      it("takes first N elements") {
        assert(List.take(List(1, 2, 3, 4), 3) == List(1, 2, 3))
      }

      it("can not take more than there is") {
        assert(List.take(List(1, 2, 3, 4), 5) == List(1, 2, 3, 4))
      }

      it("can also take nothing") {
        assert(List.take(List(1, 2, 3, 4), 0) == Nil)
        assert(List.take(List(1, 2, 3, 4), -10) == Nil)
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
        assert(List.dropWhile(Nil, (_: Int) => true) == Nil)
      }

      it("works with false predicate") {
        assert(List.dropWhile(List(1, 2, 3, 4), (_: Int) => false) == List(1, 2, 3, 4))
      }

      it("works with true predicate") {
        assert(List.dropWhile(List(1, 2, 3, 4), (_: Int) => true) == Nil)
      }

      it("drops while predicate is true") {
        assert(List.dropWhile(List(1, 2, 3, 4), (x: Int) => x <= 2) == List(3, 4))
      }
    }

    describe("init") {
      it("works on empty list") {
        assert(List.init(Nil) == Nil)
      }

      it("works for one-element lists") {
        assert(List.init(List(1)) == Nil)
      }

      it("returns everything but the last element") {
        assert(List.init(List(1, 2, 3, 4)) == List(1, 2, 3))
      }
    }

    describe("productFold") {
      it("works on empty list") {
        assert(List.productFoldRight(Nil) == 1)
      }

      it("multiplies of elements of the list") {
        assert(List.productFoldRight(List(1, 2, 3, 4)) == 24)
      }

      it("handles zero") {
        assert(List.productFoldRight(List(1, 2, 0, 4)) == 0)
      }
    }

    describe("length") {
      it("works on empty list") {
        assert(List.length(Nil) == 0)
      }

      it("counts the number of elements in non-empty list") {
        assert(List.length(List(1, 2, 3)) == 3)
      }
    }

    describe("foldLeft") {
      def sum(as: List[Int]): Int =
        List.foldLeft(as, 0)(_ + _)

      it("works on empty list") {
        assert(sum(Nil) == 0)
      }

      it("works on even number of elements") {
        assert(sum(List(1, 2, 3, 4)) == 10)
      }

      it("works on odd number of elements") {
        assert(sum(List(1, 2, 3)) == 6)
      }

      // What's the reason it doesn't?
      //    it("works for very large lists") {
      //      val bigNumbers = 1 to 10000
      //      assert(sum(IList(bigNumbers: _*)) == 100)
      //    }
    }

    describe("sumFoldLeft") {
      it("works for empty list") {
        assert(List.sumFoldLeft(Nil) == 0)
      }

      it("sums up integers") {
        assert(List.sumFoldLeft(List(1, 2, 3, 4)) == 10)
      }
    }

    describe("productFoldRight") {
      it("works on empty list") {
        assert(List.productFoldRight(Nil) == 1)
      }

      it("multiplies of elements of the list") {
        assert(List.productFoldRight(List(1, 2, 3, 4)) == 24)
      }

      it("handles zero") {
        assert(List.productFoldRight(List(1, 2, 0, 4)) == 0)
      }
    }

    describe("reverse") {
      it("works on empty list") {
        assert(List.reverse(Nil) == Nil)
      }

      it("works on single element lists") {
        assert(List.reverse(List(1)) == List(1))
      }

      it("reverses list") {
        assert(List.reverse(List(1, 2, 3)) == List(3, 2, 1))
      }
    }

    describe("append") {
      it("works on empty list") {
        assert(List.append(List.empty[Int], 1) == List(1))
      }

      it("appens element to a list") {
        assert(List.append(List(1, 2, 3), 4) == List(1, 2, 3, 4))
      }
    }

    describe("union") {
      it("works with empty lists") {
        assert(List.union(List.empty[Int], List.empty[Int]) == List.empty[Int])
      }

      it("works with empty list on the left") {
        assert(List.union(List.empty[Int], List(1, 2, 3)) == List(1, 2, 3))
      }

      it("works with empty list on the right") {
        assert(List.union(List(1, 2, 3), List.empty[Int]) == List(1, 2, 3))
      }

      it("concatenates lists") {
        assert(List.union(List(1, 2, 3), List(4, 5, 6)) == List(1, 2, 3, 4, 5, 6))
      }
    }

    describe("plusOne") {
      it("works with empty lists") {
        assert(List.plusOne(Nil) == Nil)
      }

      it("adds one") {
        assert(List.plusOne(List(1, 2, 3)) == List(2, 3, 4))
      }
    }

    describe("doubleToString") {
      it("works with empty lists") {
        assert(List.doubleToString(Nil) == Nil)
      }

      it("adds one") {
        assert(List.doubleToString(List(1, 2, 3)) == List("1", "2", "3"))
      }
    }

    describe("map") {
      it("works on empty lists") {
        assert(List.map(List.empty[Int])(_ + 1) == List.empty[Int])
      }

      it("maps a function over a list") {
        assert(List.map(List(1, 2, 3))(_ + 1) == List(2, 3, 4))
      }
    }

    describe("filter") {
      it("works on empty list") {
        assert(List.filter(List.empty[Int])(_ % 2 == 0) == List.empty[Int])
      }

      it("filters elements which do not match predicate") {
        assert(List.filter(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0) == List(2, 4, 6))
      }
    }

    describe("flatMap") {
      it("works on empty list") {
        assert(List.flatMap(List.empty[Int])(_ => List(1, 2, 3)) == List.empty[Int])
      }

      it("maps and flattens") {
        val multiplier123 = (x: Int) => List.map(List(1, 2, 3))(_ * x)

        assert(List.flatMap(List(1, 2, 3))(multiplier123) == List(1, 2, 3, 2, 4, 6, 3, 6, 9))
      }
    }

    describe("filterFlatMap") {
      it("works on empty list") {
        assert(List.filterFlatMap(List.empty[Int])(_ % 2 == 0) == List.empty[Int])
      }

      it("filters elements which do not match predicate") {
        assert(List.filterFlatMap(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0) == List(2, 4, 6))
      }
    }

    describe("sumElements") {
      it("works on empty lists") {
        assert(List.sumElements(List.empty[Int], List.empty[Int]) == List.empty[Int])
      }

      it("sums corresponding pairs") {
        assert(List.sumElements(List(1, 2, 3), List(4, 5, 6)) == List(5, 7, 9))
      }

      it("dies if lengths don't match") {
        assertThrows[RuntimeException] {
          List.sumElements(List(1, 2, 3), List(4, 5))
        }
      }
    }

    describe("zipWith") {
      it("works on empty lists") {
        assert(List.zipWith(List.empty[Int], List.empty[Int])((_, _)) == List.empty[(Int, Int)])
      }

      it("sums corresponding pairs") {
        assert(List.zipWith(List(1, 2, 3), List(4, 5, 6))((_, _)) == List((1, 4), (2, 5), (3, 6)))
      }

      it("dies if lengths don't match") {
        assertThrows[RuntimeException] {
          List.zipWith(List(1, 2, 3), List(4, 5))((_, _))
        }
      }
    }

    describe("hasSubsequence") {
      it("works on empty lists") {
        assert(List.hasSubsequence(Nil, Nil))
      }

      it("nothing is subset of anything") {
        assert(List.hasSubsequence(List(1, 2, 3), Nil))
      }

      it("nothing is subset of empty set") {
        assert(!List.hasSubsequence(Nil, List(1, 2, 3)))
      }

      it("bigger subset can not be subsequence of smaller one") {
        assert(!List.hasSubsequence(List(1, 2, 3), List(1, 2, 3, 4)))
      }

      it("detect subsets") {
        assert(List.hasSubsequence(List(1, 2, 3, 4), List(2, 3)))
      }

      it("detects subsets on left boundary") {
        assert(List.hasSubsequence(List(1, 2, 3, 4), List(1, 2)))
      }

      it("detects subsets on right boundary") {
        assert(List.hasSubsequence(List(1, 2, 3, 4), List(3, 4)))
      }

      it("correctly detects absence on subset") {
        assert(!List.hasSubsequence(List(1, 2, 3, 4), List(2, 5)))
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
