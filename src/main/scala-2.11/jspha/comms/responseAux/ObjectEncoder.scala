/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package jspha.comms.responseAux

import scala.language.higherKinds
import io.circe
import io.circe.{Encoder, JsonObject}
import jspha.comms._
import shapeless._
import shapeless.labelled._

import scala.collection.immutable.HashMap

trait ObjectEncoder[T <: HList] extends circe.ObjectEncoder[T]

object ObjectEncoder {

  implicit val ofHNil: ObjectEncoder[HNil] = new ObjectEncoder[HNil] {
    def encodeObject(a: HNil): JsonObject = JsonObject.empty
  }

  implicit def ofAtomicHCons[K <: Symbol, P, A, F[_], T <: HList](
      implicit oeT: ObjectEncoder[T],
      kWit: Witness.Aux[K],
      aEnc: circe.Encoder[F[A]],
      pEnc: circe.KeyEncoder[P]
  ): ObjectEncoder[FieldType[K, HashMap[P, F[A]]] :: T] =
    new ObjectEncoder[FieldType[K, HashMap[P, F[A]]] :: T] {
      def encodeObject(a: FieldType[K, HashMap[P, F[A]]] :: T) = {
        val valueEncoder: circe.Encoder[HashMap[P, F[A]]] =
          circe.Encoder.encodeMapLike[HashMap, P, F[A]](pEnc, aEnc)
        val name = kWit.value.name
        (name -> valueEncoder(a.head)) +: oeT.encodeObject(a.tail)
      }
    }

  implicit def ofNestedHCons[K <: Symbol, P, N[_ <: Spec], F[_], T <: HList](
      implicit oeT: ObjectEncoder[T],
      kWit: Witness.Aux[K],
      nEnc: Lazy[HasEncoder[N]],
      pEnc: circe.KeyEncoder[P],
      fSubs: F[Response[N]] <:< CSet[Response[N]]
  ): ObjectEncoder[FieldType[K, HashMap[P, F[Response[N]]]] :: T] =
    new ObjectEncoder[FieldType[K, HashMap[P, F[Response[N]]]] :: T] {
      def encodeObject(a: FieldType[K, HashMap[P, F[Response[N]]]] :: T) = {
        val valueEncoder: circe.Encoder[HashMap[P, F[Response[N]]]] =
          circe.Encoder.encodeMapLike[HashMap, P, F[Response[N]]](
            pEnc,
            CSet.hasEncoder(nEnc.value).contramap(fSubs)
          )
        val name = kWit.value.name
        (name -> valueEncoder(a.head)) +: oeT.encodeObject(a.tail)
      }
    }

}
