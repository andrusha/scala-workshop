package exercies
import scala.{Option => _}

/**
  * Error handling with exceptions is context-dependent and isn't type-safe,
  * as the function which throws exception is partial function, ie
  * it's only defined for some of the values of input types
  *
  * Here we'll find a way to consolidate and centralize error-handling logic
  * without using exceptions
  */
object ErrorHandling {

  /**
    * Sometimes your function is not defined for all possible input values,
    * in that case it might be useful to denote that no return value could exist
    *
    *   Option a = Some a | None
    *
    * eg:
    *   partial function:
    *     def max(List[Int]): Int
    *     max(List.empty) => java.lang.UnsupportedOperationException
    *
    *   total function:
    *     def max(List[Int]): Option[Int]
    *     max(List.empty) => None
    *
    * We call function partial if it doesn't have return value
    * for all the possible values of input type. Conversely we call
    * function total if it has return value for any possible input value
    */
  sealed trait Option[+A] {
    /**
      * Just as with lists Option could be seen as a "container"
      * and therefore could be mapped
      *
      * @param f mapper
      * @return transformed contents of option
      */
    def map[B](f: A => B): Option[B] = this match {
      case Some(get) => Some(f(get))
      case None => None
    }

    /**
      * @param f mapper
      * @return transformed contents of option
      */
    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case Some(get) => f(get)
      case None => None
    }

    /**
      * Useful when you want to extract contents of option, notice:
      *   B >: A means B is supertype of A
      *   () => B will ensure that B is not executed unless needed,
      *           ie it's laziness modifier
      *
      * Returning default value makes "getting" something a total function
      *
      * @param default
      * @return contained or default value
      */
    def getOrElse[B >: A](default: => B): B = this match {
      case Some(get) => get
      case None => default
    }

    /**
      * @param that alternative path
      * @return alternative option if main one is empty
      */
    def orElse[B >: A](that: => Option[B]): Option[B] = this match {
      case Some(_) => this
      case None => that
    }

    /**
      * @param f predicate
      * @return filtered contents of option
      */
    def filter(f: A => Boolean): Option[A] = this match {
      case Some(get) if f(get) => this
      case _ => None
    }
  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  object Option {
    def empty[A]: Option[A] = None

    def apply[A](a: A): Option[A] =
      if (a == null) None
      else Some(a)
  }

  /**
    * @param ds
    * @return mean value if sequence is not empty
    */
  def mean(ds: Seq[Double]): Option[Double] = ds match {
    case Nil => None
    case _ => Some(ds.sum / ds.length)
  }

  /**
    * Variance is defined as:
    *   𝛔² = 𝚺(x - m)² / 𝑵
    *  , where:
    *    m - mean of the sequence
    *    N - number of elements
    *
    * @param ds
    * @return variance of the sequence
    */
  def variance(ds: Seq[Double]): Option[Double] = mean(ds).map { m =>
      ds.map(_ - m).map(math.pow(_, 2)).sum / ds.length
    }

  /**
    * Sometimes your function is not just defined or undefined
    * for given input value, instead it might result in an
    * "exceptional" situation, which needs to have special handling.
    *
    * Try allows you to handle that, ie "lift" effectful code, which
    * throws exceptions into safe territory, eg:
    *
    *   @ Try(123 / 0)
    *   res1: Try[Int] = Failure(java.lang.ArithmeticException: / by zero)
    */
  sealed trait ITry[+A]
  case class ISuccess[+A](get: A) extends ITry[A]
  case class IFailure[+A](t: Throwable) extends ITry[A]

  object ITry {
    import scala.util.control.NonFatal

    def apply[A](f: => A): ITry[A] = {
      try {
        ISuccess(f)
      } catch {
        case NonFatal(e) => IFailure(e)
      }
    }
  }


  /**
    * You have noticed and rightfully generalized, that
    * what we were doing is returning _either_ value or nothing or
    * _either_ value or exception.
    *
    * When Either is used for error handling it's traditionally
    * agreed that _left_ is an error and _right_ is a value, eg:
    *
    *   def buy(coffee: Coffee, creditCard: CreditCard): Either[TransactionError, Transaction]
    *
    * However Either can be used not just for error handling,
    * but for any case when two paths can be taken, eg:
    *
    *   def party(person: Person): Either[DrunkPerson, WastedPerson]
    *
    * @tparam A left type
    * @tparam B right type
    */
  sealed trait IEither[+A, +B]
  case class ILeft[+A](get: A) extends IEither[A, Nothing]
  case class IRight[+B](get: B) extends IEither[Nothing, B]
}
