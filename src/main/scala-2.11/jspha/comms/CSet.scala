/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package jspha.comms

import cats.Functor
import cats.data.Xor
import io.circe.{Decoder, Encoder, Json}
import jspha.comms.util.Narrowing

/**
  * A CSet, "cardinal set", is a collection of values with a known
  * cardinality limit.
  */
sealed trait CSet[A] {

  type Size <: Cardinality

  def narrow[C <: Cardinality](implicit narrows: Narrowing[C]): Option[C#Wrap[A]] =
    narrows(this)

  def map[B](f: A => B): CSet[B]

}

object CSet {

  import cats.instances.all._
  import cats.syntax.functor._
  import io.circe.syntax._

  def sing[A](value: A): CSet[A] = Singular(value)
  def opt[A](value: Option[A]): CSet[A] = Optional(value)
  def vary[A](value: List[A]): CSet[A] = Variable(value)

  implicit val isFunctor: Functor[CSet] = new Functor[CSet] {
    def map[A, B](fa: CSet[A])(f: A => B): CSet[B] = fa.map(f)
  }

  implicit def hasEncoder[A](implicit enc: Encoder[A]): Encoder[CSet[A]] =
    new Encoder[CSet[A]] {
      def apply(a: CSet[A]): Json = a match {
        case b @ Singular(_) => b.asJson
        case b @ Optional(_) => b.asJson
        case b @ Variable(_) => b.asJson
      }
    }

  implicit def hasDecoder[A](implicit dec: Decoder[A]): Decoder[CSet[A]] =
    Singular.hasDecoder[A] or
      (Optional.hasDecoder[A] or
        Variable.hasDecoder[A].widen[CSet[A]])

  case class Singular[A](value: A) extends CSet[A] {
    type Size = Cardinality.Singular
    def map[B](f: A => B) = copy(f(value))
  }

  object Singular {
    val name = "Singular"
    implicit val isFunctor: Functor[Singular] = new Functor[Singular] {
      def map[A, B](fa: Singular[A])(f: A => B): Singular[B] = fa.map(f)
    }
    implicit def hasEncoder[A](implicit enc: Encoder[A]): Encoder[Singular[A]] =
      new Encoder[Singular[A]] {
        def apply(a: Singular[A]): Json = Json.obj(name -> enc(a.value))
      }
    implicit def hasDecoder[A](implicit dec: Decoder[A]): Decoder[Singular[A]] =
      Decoder.decodeJsonObject.emap { o =>
        o(name) match {
          case None => Xor.Left(s"Expecting object with key '$name'")
          case Some(j) => dec.decodeJson(j).bimap(_.message, Singular(_))
        }
      }
  }

  case class Optional[A](value: Option[A]) extends CSet[A] {
    type Size = Cardinality.Optional
    def map[B](f: A => B) = copy(value map f)
  }

  object Optional {
    val name = "Optional"
    implicit val isFunctor: Functor[Optional] = new Functor[Optional] {
      def map[A, B](fa: Optional[A])(f: A => B): Optional[B] = fa.map(f)
    }
    implicit def hasEncoder[A](implicit enc: Encoder[A]): Encoder[Optional[A]] =
      new Encoder[Optional[A]] {
        def apply(a: Optional[A]): Json =
          Json.obj(name -> Encoder.encodeOption(enc)(a.value))
      }
    implicit def hasDecoder[A](implicit dec: Decoder[A]): Decoder[Optional[A]] =
      Decoder.decodeJsonObject.emap { o =>
        o(name) match {
          case None => Xor.Left(s"Expecting object with key '$name'")
          case Some(j) =>
            Decoder.decodeOption(dec).decodeJson(j).bimap(
              _.message,
              Optional(_)
            )
        }
      }
  }

  case class Variable[A](value: List[A]) extends CSet[A] {
    type Size = Cardinality.Variable
    def map[B](f: A => B) = copy(value map f)
  }

  object Variable {
    val name = "Variable"
    implicit val isFunctor: Functor[Variable] = new Functor[Variable] {
      def map[A, B](fa: Variable[A])(f: A => B): Variable[B] = fa.map(f)
    }
    implicit def hasEncoder[A](implicit enc: Encoder[A]): Encoder[Variable[A]] =
      new Encoder[Variable[A]] {
        private val encList: Encoder[List[A]] =
          Encoder.encodeFoldable[List, A](enc, implicitly)
        def apply(a: Variable[A]): Json =
          Json.obj(name -> encList(a.value))
      }
    implicit def hasDecoder[A](implicit dec: Decoder[A]): Decoder[Variable[A]] = {
      val listDec: Decoder[List[A]] =
        Decoder.decodeCanBuildFrom[A, List](dec, implicitly)
      Decoder.decodeJsonObject.emap { o =>
        o(name) match {
          case None => Xor.Left(s"Expecting object with key '$name'")
          case Some(j) =>
            listDec.decodeJson(j).bimap(_.message, Variable(_))
        }
      }
    }
  }
}
