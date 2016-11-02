/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package jspha.comms

import cats.Functor
import io.circe._
import io.circe.generic.semiauto._

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

  implicit def makeEncoder[A](implicit enc: Encoder[A]): Encoder[MSet[A]] =
    deriveEncoder[MSet[A]]

  implicit def makeDecoder[A](implicit enc: Decoder[A]): Decoder[MSet[A]] =
    deriveDecoder[MSet[A]]

}
