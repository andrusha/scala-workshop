package solutions

import scala.annotation.tailrec

/**
  * Better solutions are welcome!
  */
object Functions {
  def fib(n: Int): Long = {
    @tailrec
    def loop(n: Int, prev: Long, current: Long): Long = {
      if (n == 0) {
        current
      } else {
        loop(n - 1, current, current + prev)
      }
    }

    loop(n, 1, 0)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int, prev: A, sorted: Boolean): Boolean = {
      val current = as(n)
      val result = sorted && ordered(prev, current)

      if (n == as.length - 1) {
        result
      } else {
        loop(n + 1, current, result)
      }
    }

    if (as.isEmpty || as.length == 1) {
      true
    } else {
      loop(1, as.head, true)
    }
  }

  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    x: A => y: B => f(x, y)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (x: A, y: B) => f(x)(y)
  }

  def compose[A, B, C](f: A => B, g: B => C): A => C = {
    x: A => g(f(x))
  }
}
