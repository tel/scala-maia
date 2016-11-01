package jspha.comms

import cats.instances.list._
import cats.instances.option._
import cats.{Applicative, Eval, Traverse}
import io.circe.Decoder

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable
import scala.language.higherKinds

/**
  * A `Multiplicity` is a mode in which a type can be fetched. The most basic
  * Multiplicity is `One` which means that a single item definitely can be
  * fetched. Beyond this there is `ZeroOrOne` which means that the item may
  * not exist and `Many` which means pretty much what it says.
  */
sealed trait Mult {
  type Apply[T]
}

object Mult {
  sealed trait One extends Mult { type Apply[T] = T }
  sealed trait ZeroOrOne extends Mult { type Apply[T] = Option[T] }
  sealed trait Many extends Mult { type Apply[T] = List[T] }

  object instances {
    implicit val OneApplyTraverse: Traverse[One#Apply] =
      new Traverse[One#Apply] {
        def traverse[G[_], A, B](fa: A)(f: A => G[B])(
            implicit ev: Applicative[G]): G[B] =
          f(fa)

        def foldLeft[A, B](fa: A, b: B)(f: (B, A) => B): B =
          f(b, fa)

        def foldRight[A, B](fa: A, lb: Eval[B])(
            f: (A, Eval[B]) => Eval[B]): Eval[B] =
          f(fa, lb)
      }
    implicit val ZeroOrOneApplyTraverse: Traverse[ZeroOrOne#Apply] =
      implicitly[Traverse[Option]]
    implicit val ManyApplyTraverse: Traverse[Many#Apply] =
      implicitly[Traverse[List]]
  }

  /**
    * Eliminates an MSet with the right type.
    */
  trait MSelector[M <: Mult] {
    def apply[T](m: MSet[T]): Option[M#Apply[T]]
    def embed[T](s: M#Apply[T]): MSet[T]
    def decoder[A](implicit decoderA: Decoder[A]): Decoder[M#Apply[A]]
  }

  object MSelector {
    implicit val MSelectorOne: MSelector[One] = new MSelector[One] {
      def apply[T](m: MSet[T]): Option[T] = m match {
        case MSet.One(t) => Some(t)
        case _ => None
      }

      def embed[T](s: T): MSet[T] = MSet.One(s)

      def decoder[A](implicit decoderA: Decoder[A]): Decoder[A] = decoderA
    }
    implicit val MSelectorZeroOrOne: MSelector[ZeroOrOne] =
      new MSelector[ZeroOrOne] {
        def apply[T](m: MSet[T]): Option[Option[T]] = m match {
          case MSet.ZeroOrOne(t) => Some(t)
          case _ => None
        }

        def embed[T](s: Option[T]): MSet[T] = MSet.ZeroOrOne(s)

        def decoder[A](implicit decoderA: Decoder[A]): Decoder[Option[A]] =
          Decoder.decodeOption(decoderA)
      }
    implicit val MSelectorMany: MSelector[Many] = new MSelector[Many] {
      def apply[T](m: MSet[T]): Option[List[T]] = m match {
        case MSet.Many(t) => Some(t)
        case _ => None
      }

      def embed[T](s: List[T]): MSet[T] = MSet.Many(s)

      def decoder[A](implicit decoderA: Decoder[A]): Decoder[List[A]] =
        Decoder.decodeCanBuildFrom(decoderA, implicitly)
    }
  }
}
