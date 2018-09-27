package exercies

import scala.annotation.tailrec

object Functions {
  /**
    * Fibonacci sequence is the sum of previous two numbers:
    *   0, 1, 1, 2, 3, 5, ...
    * First numbers of the sequence are always 0 and 1.
    *
    * It could be defined mathematically as such:
    *   F(0) = 0, F(1) = 1
    *   F(n) = F(n - 1) + F(n - 2)
    *
    * Use tail-recursion, in order to ensure that it's correct use @tailrec annotation:
    *
    * @tailrec
    * def sum(n: Int, acc: Long): Long = {
    *   if (n == 0) {
    *     0
    *   } else {
    *     sum(n - 1, n + acc)
    *   }
    * }
    *
    * @param n sequence index
    * @return then n-th Fibonacci
    */
  def fib(n: Int): Long = ???

  /**
    * Monomorphic function is a function which operates on one specific type:
    *   def headInt(ints: Array[Int]): Int
    *
    * Polymorphic function is a function which works on a set of types, ie
    * generic over the type parameter:
    *   def head[A](as: Array[A]): A
    *
    * High-order function (HOF) is a function which either takes
    * a function as a parameter or returns function as a result:
    *   def sum[A](as: Array[A], summator: (A, A) => A): A
    *
    * Arrays can be accessed by index like this `as(i)` with complexity O(1)
    *
    * @param as array of things to check for ordering
    * @param ordered function which knows how to compare pairs of values
    * @return
    */
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = ???

  /**
    * Another example of a HOF is curry which both accepts and returns a function
    *
    * Currying is the process of converting function of multiple arguments to the
    * function of one argument, which returns a function as a result.
    *
    * Any function can be curried, multi-parameter functions could be seen as a "syntax-sugar".
    *
    * Applying curried function to only some of its arguments is called Partial Application:
    *   val adder = (x: Int) => (y: Int) => x + y
    *   val addTwo = adder(2)
    *   addTwo(2) == 4
    *
    * Ref:
    *   https://en.wikipedia.org/wiki/Currying
    *
    * @param f two-argument function to be curried
    * @return curried version of f
    */
  def curry[A, B, C](f: (A, B) => C): A => B => C = ???

  /**
    * Uncurry dual (inverse) transformation of curry function
    *
    * @param f curried function
    * @return uncurried version of f
    */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = ???

  /**
    * Composition allows you to build complex programs from small reusable parts.
    *
    * One of the practical applications of functional composition is so called "pipelining":
    *   list.map(f).map(g) == list.map(f.compose(g))
    *
    * Ref:
    *   https://en.wikipedia.org/wiki/Function_composition_(computer_science)
    *
    * @param f
    * @param g
    * @return
    */
  def compose[A, B, C](f: A => B, g: B => C): A => C = ???
}
