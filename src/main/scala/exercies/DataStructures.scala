package exercies

import scala.annotation.tailrec

object DataStructures {

  /**
    * Single linked list could be modeled as sum type (algebraic data type), ie
    * list is either empty or element with the link to a next one, eg:
    *   data List a = Empty | Cons a (List a)
    *
    *   Cons 1 (Cons 2 (INil)) = [1, 2]
    */
  sealed trait IList[+A]
  case object INil extends IList[Nothing]
  case class ICons[+A](head: A, tail: IList[A]) extends IList[A]

  object IList {
    /**
      * It's convenience method, as empty element is the same for all
      * Lists (Nothing is bottom type of any type), but we have to
      * hint a compiler to know to which list it should belong to
      *
      * @return INil
      */
    def empty[A]: IList[A] = IList[A]()

    /**
      * Syntax `A*` defines variadic function, or a function, which takes
      * variable number of arguments. Those are all valid calls:
      *   IList.apply()
      *   IList(1)
      *   IList("a", "b", "c")
      *
      * The direct application of an object `IList()` is a sugar to `apply` itself `IList.apply()`
      *
      * The `: _*` construct destructs list to the arguments
      *
      * Notice that this is not tail recursive function, there fore you can run out of stack
      *
      * @param as list of objects to create list from
      * @return a list of objects
      */
    def apply[A](as: A*): IList[A] = {
      if (as.isEmpty) INil
      else ICons(as.head, apply(as.tail: _*))
    }

    /**
      * Pattern matching deconstructs given value into its parts
      *
      * @param ints to sum
      * @return sum of all the elements in the list
      */
    def sum(ints: IList[Int]): Int = ints match {
      case INil => 0
      case ICons(x, xs) => x + sum(xs)
    }

    /**
      * @param ds
      * @return the result of all elements of ds multiplied together
      */
    def product(ds: IList[Double]): Double = ???

    /**
      * @param as
      * @return as without the first element
      */
    def tail[A](as: IList[A]): IList[A] = ???

    /**
      * @param as
      * @param n number of elements to remove
      * @return as without first n elements
      */
    def drop[A](as: IList[A], n: Int): IList[A] = ???

    /**
      * Implement function which drops elements while `f` holds
      *
      * @param as
      * @param f predicate
      * @return remainder of as filtered by f
      */
    def dropWhile[A](as: IList[A], f: A => Boolean): IList[A] = ???

    /**
      * Implement "opposite" of tail
      *
      * @param as
      * @return as without the very last element
      */
    def init[A](as: IList[A]): IList[A] = ???

    /**
      * Advanced, will be useful for other advanced tasks
      *
      * @param as
      * @param n how many elements to take
      * @return first n elements of the given list
      */
    def take[A](as: IList[A], n: Int): IList[A] = ???

    /**
      * Fold is one of the patterns of generalized recursion,
      * which collapses your data structure starting from the right:
      *
      * x1 :: x2 :: x3 :: Nil
      *  \     \    -  f x3 z
      *  \      -  f x2 result
      *   ---- f x1 result
      *       result
      *
      * Due to this structure it can not be made tail-recursive
      *
      * Notice that this version is curried, this will allow
      * compiler to infer correct types for the `f`
      *
      * @param as list of things to fold
      * @param z seed
      * @param f combinator
      * @return folded list using f
      */
    def foldRight[A, B](as: IList[A], z: B)(f: (A, B) => B): B = ???

    /**
      * Implement product using foldRight method
      *
      * Is it possible to short-circuit this implementation
      * if 0 is encountered?
      *
      * @param ds
      * @return the result of all elements of ds multiplied together
      */
    def productFoldRight(ds: IList[Double]): Double = ???

    /**
      * Implement length using foldRight
      *
      * Will it fail for large lists?
      *
      * @param as
      * @return number of elements in the list
      */
    def length[A](as: IList[A]): Int = ???

    /**
      * Fold which is running from left to right is possible
      * to implement in tail-recursive manner
      *
      * x1 :: x2   ::   x3    ::   Nil
      * f x1 z \        \          \
      *     f x2 result  \         \
      *               f x3 result  \
      *                        result
      *
      * Is it possible to short-circuit fold?
      * Is it possible to express foldRight using foldLeft?
      *
      * @param as list of things to fold
      * @param z seed
      * @param f combinator
      * @return folded list using f
      */
    def foldLeft[A, B](as: IList[A], z: B)(f: (A, B) => B): B = ???

    /**
      * Implement sum using fold pattern
      *
      * @return sum of all the elements in the list
      */
    def sumFoldLeft(ds: IList[Int]): Int = ???

    /**
      * Implement product using fold pattern
      *
      * @param ds
      * @return
      */
    def productFoldLeft(ds: IList[Int]): Int = ???

    /**
      * Implement list reversal using fold
      */
    def reverse[A](as: IList[A]): IList[A] = ???

    /**
      * Implement it in linear time (is it possible to do it in constant time?)
      *
      * @param as
      * @param a element to append
      * @return list with appended element
      */
    def append[A](as: IList[A], a: A): IList[A] = ???

    /**
      * Preferred complexity O(n + m)
      *
      * @param xs left list
      * @param ys right list
      * @return xs + ys
      */
    def union[A](xs: IList[A], ys: IList[A]): IList[A] = ???

    /**
      * @param xs
      * @return one added to each element in the list
      */
    def plusOne(xs: IList[Int]): IList[Int] = ???

    /**
      * @param ds
      * @return list doubles transformed to strings
      */
    def doubleToString(ds: IList[Double]): IList[String] = ???

    /**
      * Is it possible to implement it in tail-recursive manner? Using fold?
      *
      * @param as list to transform
      * @param f transformator
      * @return list with all the elements being transformed by f
      */
    def map[A, B](as: IList[A])(f: A => B): IList[B] = ???

    /**
      * @param as
      * @param f predicate
      * @return list filtered by predicate f
      */
    def filter[A](as: IList[A])(f: A => Boolean): IList[A] = ???

    /**
      * Flat Map is equivalent to .map.flatten
      *
      * What's the most efficient implementation of it?
      *
      * @param as
      * @param f transformator which returns a list itself
      * @return all the transformed lists chained together
      */
    def flatMap[A, B](as: IList[A])(f: A => IList[B]): IList[B] = ???

    /**
      * Implement filtering using flatMap
      *
      * @param as
      * @param f predicate
      * @return list filtered by predicate f
      */
    def filterFlatMap[A](as: IList[A])(f: A => Boolean): IList[A] = ???

    /**
      * Sum each corresponding pair from `as` and `bs`
      *
      * If behaviour is undefined you can throw an exception for now with:
      *   sys.error("something")
      *
      * Later we'll learn how to handle errors in functional manner
      *
      * @param as
      * @param bs
      * @return a list of sum of pairs from corresponding lists
      */
    def sumElements(as: IList[Int], bs: IList[Int]): IList[Int] = ???

    /**
      * @param as
      * @param bs
      * @param f combiner
      * @return pairs of elements combined together with f
      */
    def zipWith[A, B, C](as: IList[A], bs: IList[B])(f: (A, B) => C): IList[C] = ???

    /**
      * Advanced
      *
      * Implement function which looks for subsequence of the list inside the other list
      * It doesn't have to be as efficient as possible, but keep it functional
      *
      * @param sup list we're looking for subsequences in
      * @param sub subsequence
      * @return if subsequence was found or not
      */
    def hasSubsequence[A](sup: IList[A], sub: IList[A]): Boolean = ???
  }

  /**
    * Using algebraic data types you can express even complex structures than lists.
    * Here we'll implement binary tree using the same techniques we've learned before.
    */
  sealed trait Tree[+A]
  case object EmptyLeaf extends Tree[Nothing]
  case class Leaf[+A](value: A) extends Tree[A]
  case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    def emptyLeaf[A]: Tree[A] = EmptyLeaf

    /**
      * @param t
      * @return number of all the Leafs and Branches
      */
    def size[A](t: Tree[A]): Int = ???

    /**
      * @param ti
      * @return biggest element in a tree
      */
    def maxiumum(ti: Tree[Int]): Int = ???

    /**
      * @param t
      * @return a deepest path towards a leaf
      */
    def depth[A](t: Tree[A]): Int = ???

    /**
      * Trees can also be transformed in the same way we transformed lists
      *
      * Can it be made tail-recursive?
      *
      * @param t
      * @param f transformer
      * @return new tree with leaf values transformed by f
      */
    def map[A, B](t: Tree[A])(f: A => B): Tree[B] = ???

    /**
      * Folding over trees is a bit more involved, as you have to combine
      * intermediate values somehow to get the end result
      *
      * Notice that we're developing pattens similar to before
      *
      * @param t subject
      * @param z seed
      * @param f folder
      * @param combine combiner
      * @return tree folded to a single value
      */
    def fold[A, B](t: Tree[A], z: B)(f: (A, B) => B)(combine: (B, B) => B): B = ???

    /**
      * Implement size using fold
      *
      * @param t
      * @return number of all the Leafs and Branches
      */
    def sizeFold[A](t: Tree[A]): Int = ???

    /**
      * Implement maximum using fold
      *
      * @param ti
      * @return biggest element in a tree
      */
    def maxiumumFold(ti: Tree[Int]): Int = ???

    /**
      * Implement depth using fold
      *
      * @param t
      * @return a deepest path towards a leaf
      */
    def depthFold[A](t: Tree[A]): Int = ???

    /**
      * Advanced
      * map can be generalized using fold also
      *
      * @param t
      * @param f transformer
      * @return new tree with leaf values transformed by f
      */
    def mapFold[A, B](t: Tree[A])(f: A => B): Tree[B] = ???
  }

}
