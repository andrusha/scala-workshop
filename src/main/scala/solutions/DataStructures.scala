package solutions

import exercies.DataStructures.{ICons, IList, INil}

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
}
