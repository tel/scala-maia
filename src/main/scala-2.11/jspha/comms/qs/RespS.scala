/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package jspha.comms.qs

import io.circe._
import io.circe.syntax._

import scala.language.higherKinds
import jspha.comms._
import shapeless._
import shapeless.labelled._

trait RespS extends Qs {
  type Atomic[P, M <: Mult, T] =
    Map[P, M#Apply[T]]
  type Nested[P, M <: Mult, T[_ <: Qs]] =
    Map[P, M#Apply[Response[T]]]
}

object RespS {

  trait ResponseEncoder[Api[_ <: Qs]] {
    val encoder: Encoder[Response[Api]]
  }

  object ResponseEncoder {

    def apply[Api[_ <: Qs]](
        implicit E: ResponseEncoder[Api]): ResponseEncoder[Api] = E

    implicit def GenericResponseEncoder[Api[_ <: Qs], Repr <: HList](
        implicit gen: LabelledGeneric.Aux[Response[Api], Repr],
        objectEncoder: Lazy[ObjRespEncoder[Repr]]
    ): ResponseEncoder[Api] = new ResponseEncoder[Api] {
      val encoder: Encoder[Response[Api]] =
        objectEncoder.value.contramap(gen.to)
    }

    trait ObjRespEncoder[T <: HList] extends ObjectEncoder[T]

    object ObjRespEncoder {

      implicit val HNilObjRespEncoder: ObjRespEncoder[HNil] =
        new ObjRespEncoder[HNil] {
          def encodeObject(a: HNil): JsonObject = JsonObject.empty
        }

      implicit def HConsAtomicObjRespEncoder[K <: Symbol, V, T <: HList](
          implicit objEncT: Lazy[ObjRespEncoder[T]],
          eqv: V =:= RespS#Atomic[NoParam, Mult.One, Int]
      ): ObjRespEncoder[FieldType[K, V] :: T] =
        new ObjRespEncoder[FieldType[K, V] :: T] {
          def encodeObject(a: FieldType[K, V] :: T) =
            objEncT.value.encodeObject(a.tail)
        }

    }

  }

}
