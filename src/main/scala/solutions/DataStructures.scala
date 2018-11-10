package solutions

import exercies.DataStructures._

import scala.annotation.tailrec

/**
  * Better solutions are welcome!
  */
object DataStructures {
  def product(ds: IList[Double]): Double = ds match {
    case INil => 1
    case ICons(x, xs) => x * product(xs)
  }

  def tail[A](as: IList[A]): IList[A] = as match {
    case INil => INil
    case ICons(_, t) => t
  }

  @tailrec
  def drop[A](as: IList[A], n: Int): IList[A] = as match {
    case INil => INil
    case ICons(_, _) if n <= 0 => as
    case ICons(_, t) => drop(t, n - 1)
  }

  @tailrec
  def dropWhile[A](as: IList[A], f: A => Boolean): IList[A] = as match {
    case INil => INil
    case ICons(h, t) if f(h) => dropWhile(t, f)
    case ICons(_, _) => as
  }

  def init[A](as: IList[A]): IList[A] = as match {
    case INil => INil
    case ICons(_, INil) => INil
    case ICons(h, t) => ICons(h, init(t))
  }

  def take[A](as: IList[A], n: Int): IList[A] = {
    def loop(xs: IList[A], x: Int, acc: IList[A]): IList[A] = xs match {
      case ICons(head, tail) if x > 0 => ICons(head, loop(tail, x - 1, acc))
      case _ => acc
    }

    loop(as, n, INil)
  }

  def foldRight[A, B](as: IList[A], z: B)(f: (A, B) => B): B =
    as match {
      case ICons(x, xs) => f(x, foldRight(xs, z)(f))
      case INil => z
    }

  def productFoldRight(ds: IList[Double]): Double =
    foldRight(ds, 1.0)(_ * _)

  def length[A](as: IList[A]): Int =
    foldRight(as, 0)((_, b) => 1 + b)

  @tailrec
  def foldLeft[A, B](as: IList[A], z: B)(f: (A, B) => B): B =
    as match {
      case ICons(x, xs) => foldLeft(xs, f(x, z))(f)
      case INil => z
    }

  def sumFoldLeft(ds: IList[Int]): Int =
    foldLeft(ds, 0)(_ + _)

  def productFoldLeft(ds: IList[Int]): Int =
    foldLeft(ds, 1)(_ * _)

  def reverse[A](as: IList[A]): IList[A] =
    foldLeft(as, IList.empty[A])((h, t) => ICons(h, t))

  def append[A](as: IList[A], a: A): IList[A] =
    foldRight(as, IList(a))((h, t) => ICons(h, t))

  def union[A](xs: IList[A], ys: IList[A]): IList[A] =
    foldRight(xs, ys)((h, t) => ICons(h, t))

  def plusOne(xs: IList[Int]): IList[Int] =
    foldRight(xs, IList.empty[Int])((h, t) => ICons(h + 1, t))

  def doubleToString(ds: IList[Double]): IList[String] =
    foldRight(ds, IList.empty[String])((h, t) => ICons(h.toString, t))

  def map[A, B](as: IList[A])(f: A => B): IList[B] =
    foldRight(as, IList.empty[B])((h, t) => ICons(f(h), t))

  def filter[A](as: IList[A])(f: A => Boolean): IList[A] =
    foldRight(as, IList.empty[A]) { (h, t) =>
      if (f(h))
        ICons(h, t)
      else
        t
    }
  def flatMap[A, B](as: IList[A])(f: A => IList[B]): IList[B] =
    foldRight(as, IList.empty[B])((h, t) => IList.union(f(h), t))

  def filterFlatMap[A](as: IList[A])(f: A => Boolean): IList[A] =
    flatMap(as)(a => if (f(a)) IList(a) else INil)

  def sumElements(as: IList[Int], bs: IList[Int]): IList[Int] = (as, bs) match {
    case (ICons(a, ass), ICons(b, bss)) => ICons(a + b, sumElements(ass, bss))
    case (INil, INil) => INil
    case _ => sys.error("List sizes must match")
  }

  def zipWith[A, B, C](as: IList[A], bs: IList[B])(f: (A, B) => C): IList[C] = (as, bs) match {
    case (ICons(a, ass), ICons(b, bss)) => ICons(f(a, b), zipWith(ass, bss)(f))
    case (INil, INil) => INil
    case _ => sys.error("List sizes must match")
  }

  def hasSubsequence[A](sup: IList[A], sub: IList[A]): Boolean = {
    val subLength = IList.length(sub)

    @tailrec
    def loop(xs: IList[A], p: Boolean): Boolean = xs match {
      case ICons(_, t) => IList.take(xs, subLength) == sub || loop(t, p)
      case INil => p
    }

    sub match {
      case INil => true
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
