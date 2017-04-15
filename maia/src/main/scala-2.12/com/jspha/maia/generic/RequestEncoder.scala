/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.generic

import scala.language.higherKinds
import com.jspha.maia._
import io.circe._
import shapeless._
import shapeless.labelled._

import scala.collection.immutable.HashMap

trait RequestEncoder[Api[_ <: Fields]] extends Encoder[Request[Api]]

object RequestEncoder {

  implicit def HasRequestEncoderGeneric[Api[_ <: Fields], Repr <: HList](
    implicit gen: LabelledGeneric.Aux[Request[Api], Repr],
    worker: Lazy[Worker[Repr]]
  ): RequestEncoder[Api] = new RequestEncoder[Api] {

    def apply(a: Request[Api]): Json =
      Encoder
        .encodeMapLike[HashMap, Symbol, Json]
        .apply(worker.value(gen.to(a)))

  }

  trait Worker[Repr <: HList] {
    def apply(r: Repr): HashMap[Symbol, Json]
  }

  object Worker {

    implicit val WorkerHNil: Worker[HNil] = _ => HashMap()

    implicit def WorkerAtomE[K <: Symbol, A, E, T <: HList](
      implicit recur: Worker[T],
      kWitness: Witness.Aux[K]
    ): Worker[FieldType[K, Fields.Request#AtomE[E, A]] :: T] =
      (r: FieldType[K, Fields.Request#AtomE[E, A]] :: T) => {
        val later: HashMap[Symbol, Json] = recur(r.tail)
        // NOTE: If we don't need to include a field in a request then it is
        // elided.
        if (r.head)
          later + (kWitness.value -> Json.True)
        else
          later
      }

    implicit def WorkerIAtomE[K <: Symbol, A, E, I, T <: HList](
      implicit recur: Worker[T],
      kWitness: Witness.Aux[K],
      // NOTE: We ask for KeyEncoder even though we use it as a regular
      // Encoder since we ask that indices are encodable to strings.
      iEncoder: KeyEncoder[I]
    ): Worker[FieldType[K, Fields.Request#IAtomE[I, E, A]] :: T] =
      (r: FieldType[K, Fields.Request#IAtomE[I, E, A]] :: T) => {
        val later: HashMap[Symbol, Json] = recur(r.tail)
        // NOTE: If we don't need to include a field in a request then it is
        // elided.
        if (r.head.nonEmpty)
          later + (kWitness.value -> Encoder
            .encodeSet[I](Encoder.encodeString.contramap(iEncoder.apply))
            .apply(r.head))
        else
          later
      }

    implicit def WorkerObjE[K <: Symbol,
                            A[_ <: Fields],
                            E,
                            C <: Cardinality,
                            T <: HList](
      implicit recur: Worker[T],
      recurA: Lazy[RequestEncoder[A]],
      kWitness: Witness.Aux[K]
    ): Worker[FieldType[K, Fields.Request#ObjE[C, E, A]] :: T] =
      (r: FieldType[K, Fields.Request#ObjE[C, E, A]] :: T) => {
        val later: HashMap[Symbol, Json] = recur(r.tail)
        val head: Option[Request[A]] = r.head
        head match {
          case None =>
            // NOTE: If we don't need to include a field in a request then it is
            // elided.
            later
          case Some(subReq) =>
            later + (kWitness.value -> recurA.value(subReq))
        }
      }

    implicit def WorkerIObjE[K <: Symbol,
                             I,
                             A[_ <: Fields],
                             E,
                             C <: Cardinality,
                             T <: HList](
      implicit recur: Worker[T],
      recurA: Lazy[RequestEncoder[A]],
      kWitness: Witness.Aux[K],
      iEncoder: KeyEncoder[I]
    ): Worker[FieldType[K, Fields.Request#IObjE[I, C, E, A]] :: T] =
      (r: FieldType[K, Fields.Request#IObjE[I, C, E, A]] :: T) => {
        val later: HashMap[Symbol, Json] = recur(r.tail)
        if (r.head.isEmpty)
          // NOTE: If we don't need to include a field in a request then it is
          // elided.
          later
        else
          later + (kWitness.value -> Encoder
            .encodeMapLike[Map, I, Json]
            .apply(r.head.mapValues(recurA.value.apply)))
      }

  }

}
