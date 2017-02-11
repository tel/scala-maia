/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package jspha.comms.responseAux

import cats.data.Xor

import scala.language.higherKinds
import io.circe.{Encoder, JsonObject, KeyEncoder, ObjectEncoder => OE}
import jspha.comms._
import shapeless._
import shapeless.labelled._

import scala.collection.immutable.HashMap

trait ObjectEncoder[T <: HList] extends OE[T]

object ObjectEncoder {

  implicit val ofHNil: ObjectEncoder[HNil] = new ObjectEncoder[HNil] {
    def encodeObject(a: HNil): JsonObject = JsonObject.empty
  }

  implicit def ofAtomicHCons[K <: Symbol, P, A, E, F[_], T <: HList](
      implicit oeT: ObjectEncoder[T],
      kWit: Witness.Aux[K],
      eEnc: Encoder[E],
      faEnc: Encoder[F[A]],
      pEnc: KeyEncoder[P]
  ): ObjectEncoder[FieldType[K, HashMap[P, Xor[E, F[A]]]] :: T] =
    new ObjectEncoder[FieldType[K, HashMap[P, Xor[E, F[A]]]] :: T] {
      def encodeObject(a: FieldType[K, HashMap[P, Xor[E, F[A]]]] :: T) = {
        val xorEncoder: Encoder[Xor[E, F[A]]] =
          Encoder.encodeXor("Error", "Ok")(eEnc, faEnc)
        val valueEncoder: Encoder[HashMap[P, Xor[E, F[A]]]] =
          Encoder.encodeMapLike[HashMap, P, Xor[E, F[A]]](pEnc, xorEncoder)
        val name = kWit.value.name
        (name -> valueEncoder(a.head)) +: oeT.encodeObject(a.tail)
      }
    }

  implicit def ofNestedHCons[K <: Symbol,
                             P,
                             E,
                             N[_ <: Spec],
                             F[_],
                             T <: HList](
      implicit oeT: ObjectEncoder[T],
      kWit: Witness.Aux[K],
      nEnc: Lazy[HasEncoder[N, E]],
      eEnc: Encoder[E],
      pEnc: KeyEncoder[P],
      fSubs: F[Response[N, E]] <:< CSet[Response[N, E]]
  ): ObjectEncoder[FieldType[K, HashMap[P, Xor[E, F[Response[N, E]]]]] :: T] =
    new ObjectEncoder[FieldType[K, HashMap[P, Xor[E, F[Response[N, E]]]]] :: T] {
      def encodeObject(
          a: FieldType[K, HashMap[P, Xor[E, F[Response[N, E]]]]] :: T) = {
        val xorEncoder: Encoder[Xor[E, F[Response[N, E]]]] =
          Encoder.encodeXor("Error", "Ok")(
            eEnc,
            CSet.hasEncoder(nEnc.value).contramap(fSubs)
          )
        val valueEncoder: Encoder[HashMap[P, Xor[E, F[Response[N, E]]]]] =
          Encoder.encodeMapLike[HashMap, P, Xor[E, F[Response[N, E]]]](
            pEnc,
            xorEncoder)
        val name = kWit.value.name
        (name -> valueEncoder(a.head)) +: oeT.encodeObject(a.tail)
      }
    }

}
