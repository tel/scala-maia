package jspha.comms

import cats.Functor

/**
  * An MSet is the union of all kinds of multiplicities.
  */
sealed trait MSet[T]

object MSet {

  def one[T](t: T): MSet[T] = One(t)
  def zeroOrOne[T](t: Option[T]): MSet[T] = ZeroOrOne(t)
  def many[T](t: List[T]): MSet[T] = Many(t)

  case class One[T](t: T) extends MSet[T]
  case class ZeroOrOne[T](t: Option[T]) extends MSet[T]
  case class Many[T](t: List[T]) extends MSet[T]

  implicit val MSetFunctor: Functor[MSet] = new Functor[MSet] {
    def map[A, B](fa: MSet[A])(f: A => B): MSet[B] = fa match {
      case One(t) => One(f(t))
      case ZeroOrOne(o) => ZeroOrOne(o.map(f))
      case Many(s) => Many(s.map(f))
    }
  }

}
