package exercies

import scala.annotation.tailrec

object DataStructures {

  /**
    * Single linked list could be modeled as sum type (algebraic data type), ie
    * list is either empty or element with the link to a next one, eg:
    *   data List a = Empty | Cons a (List a)
    *
    *   Cons 1 (Cons 2 (Empty)) = [1, 2]
    *
    *   or
    *
    */
  sealed trait IList[+A]
  case object INil extends IList[Nothing]
  case class ICons[+A](head: A, tail: IList[A]) extends IList[A]

  object IList {

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
      * @param as list of objects to create list from
      * @return a list of objects
      */
    def apply[A](as: A*): IList[A] = {
      if (as.isEmpty) INil
      else ICons(as.head, apply(as.tail: _*))
    }

    /**
      * Pattern matching
      *
      *
      * @param ints
      * @return
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
  }

}
