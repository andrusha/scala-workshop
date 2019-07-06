package exercies

import shapeless._

trait TsvEncoder[A] {
  def encode(a: A): List[String] = ???
}

object TsvEncoder {
  def apply[A](implicit enc: TsvEncoder[A]): TsvEncoder[A] =
    enc

  def pure[A](f: A => List[String]): TsvEncoder[A] =
    new TsvEncoder[A] {
      override def encode(a: A): List[String] =
        f(a)
    }
}

object Tsv {
  def writeTsv[A](a: List[A])(implicit enc: TsvEncoder[A]): String =
    a.map(enc.encode).map(_.mkString("\t")).mkString("\n")

  implicit val stringEncoder: TsvEncoder[String] =
    TsvEncoder.pure[String](List(_))

  implicit val intEncoder: TsvEncoder[Int] =
    TsvEncoder.pure[Int](List(_).map(_.toString))

  implicit val booleanEncoder: TsvEncoder[Boolean] =
    TsvEncoder.pure[Boolean](List(_).map(b => if(b) "true" else "false"))

  implicit val hnilEncoder: TsvEncoder[HNil] =
    TsvEncoder.pure[HNil](_ => Nil)

  implicit def hlistEncoder[H, T <: HList](
    implicit
      hEncoder: TsvEncoder[H],
      tEncoder: TsvEncoder[T]): TsvEncoder[H :: T] =
    TsvEncoder.pure[H :: T] {
      case h :: t =>
        hEncoder.encode(h) ++ tEncoder.encode(t)
    }

  implicit def genericEncoder[A, R](
    implicit
      gen: Generic[A] { type Repr = R },
      enc: TsvEncoder[R]
    ): TsvEncoder[A] =
      TsvEncoder.pure[A](a => enc.encode(gen.to(a)))
}
