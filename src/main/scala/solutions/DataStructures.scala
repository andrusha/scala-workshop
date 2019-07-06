package solutions

import exercies.DataStructures._

import scala.annotation.tailrec

/**
  * Better solutions are welcome!
  */
object DataStructures {
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  @tailrec
  def drop[A](as: List[A], n: Int): List[A] = as match {
    case Nil => Nil
    case Cons(_, _) if n <= 0 => as
    case Cons(_, t) => drop(t, n - 1)
  }

  @tailrec
  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case Cons(_, _) => as
  }

  def init[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def take[A](as: List[A], n: Int): List[A] = {
    def loop(xs: List[A], x: Int, acc: List[A]): List[A] = xs match {
      case Cons(head, tail) if x > 0 => Cons(head, loop(tail, x - 1, acc))
      case _ => acc
    }

    loop(as, n, Nil)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      case Nil => z
    }

  def productFoldRight(ds: List[Double]): Double =
    foldRight(ds, 1.0)(_ * _)

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, b) => 1 + b)

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Cons(x, xs) => foldLeft(xs, f(x, z))(f)
      case Nil => z
    }

  def sumFoldLeft(ds: List[Int]): Int =
    foldLeft(ds, 0)(_ + _)

  def productFoldLeft(ds: List[Int]): Int =
    foldLeft(ds, 1)(_ * _)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, List.empty[A])((h, t) => Cons(h, t))

  def append[A](as: List[A], a: A): List[A] =
    foldRight(as, List(a))((h, t) => Cons(h, t))

  def union[A](xs: List[A], ys: List[A]): List[A] =
    foldRight(xs, ys)((h, t) => Cons(h, t))

  def plusOne(xs: List[Int]): List[Int] =
    foldRight(xs, List.empty[Int])((h, t) => Cons(h + 1, t))

  def doubleToString(ds: List[Double]): List[String] =
    foldRight(ds, List.empty[String])((h, t) => Cons(h.toString, t))

  def map[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, List.empty[B])((h, t) => Cons(f(h), t))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, List.empty[A]) { (h, t) =>
      if (f(h))
        Cons(h, t)
      else
        t
    }
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, List.empty[B])((h, t) => List.union(f(h), t))

  def filterFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def sumElements(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Cons(a, ass), Cons(b, bss)) => Cons(a + b, sumElements(ass, bss))
    case (Nil, Nil) => Nil
    case _ => sys.error("List sizes must match")
  }

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Cons(a, ass), Cons(b, bss)) => Cons(f(a, b), zipWith(ass, bss)(f))
    case (Nil, Nil) => Nil
    case _ => sys.error("List sizes must match")
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    val subLength = List.length(sub)

    @tailrec
    def loop(xs: List[A], p: Boolean): Boolean = xs match {
      case Cons(_, t) => List.take(xs, subLength) == sub || loop(t, p)
      case Nil => p
    }

    sub match {
      case Nil => true
      case _ => loop(sup, false)
    }
  }

  def size[A](t: Tree[A]): Int = t match {
    case EmptyLeaf => 0
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maxiumum(ti: Tree[Int]): Int = {
    def loop(t: Tree[Int], acc: Int): Int = t match {
      case EmptyLeaf => Int.MinValue
      case Leaf(a) => acc.max(a)
      case Branch(left, right) => loop(left, acc).max(loop(right, acc))
    }

    loop(ti, Int.MinValue)
  }

  def depth[A](t: Tree[A]): Int = {
    def loop(t: Tree[A], acc: Int): Int = t match {
      case EmptyLeaf => 0
      case Leaf(_) => acc + 1
      case Branch(left, right) => loop(left, acc + 1).max(loop(right, acc + 1))
    }

    loop(t, 0)
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case EmptyLeaf => EmptyLeaf
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](t: Tree[A], z: B)(f: (A, B) => B)(combine: (B, B) => B): B = {
    def loop(t: Tree[A], acc: B): B = t match {
      case EmptyLeaf => acc
      case Leaf(value) => f(value, acc)
      case Branch(left, right) => combine(loop(left, acc), loop(right, acc))
    }

    loop(t, z)
  }

  def sizeFold[A](t: Tree[A]): Int =
    fold(t, 0)((_, acc) => acc + 1)(_ + _ + 1)

  def maxiumumFold(ti: Tree[Int]): Int =
    fold(ti, Int.MinValue)(_ max _)(_ max _)

  def depthFold[A](t: Tree[A]): Int =
    fold(t, 0)((_, acc) => acc + 1)(_ + 1 max _ + 1)

  def mapFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t, Tree.emptyLeaf[B])((a, _) => Leaf(f(a)))(Branch(_, _))
}
