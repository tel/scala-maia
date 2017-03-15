/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.generic

import scala.language.higherKinds
import com.jspha.maia._
import cats._
import cats.implicits._
import io.circe._
import shapeless._
import shapeless.labelled._
import io.circe.Decoder.Result

import scala.collection.immutable.HashMap

trait RequestDecoder[Api[_ <: Fields]] extends Decoder[Request[Api]]

object RequestDecoder {

  implicit def RequestDecoderGeneric[Api[_ <: Fields], Repr <: HList](
    implicit gen: LabelledGeneric.Aux[Request[Api], Repr],
    worker: Lazy[Worker[Repr]],
    keyList: KeyList[Repr]
  ): RequestDecoder[Api] =
    new RequestDecoder[Api] {
      // NOTE: We can (and must) handle parsing a request lacking some keys,
      // but we should error out early if a request contains keys which we do
      // not know how to serve!
      def apply(c: HCursor): Decoder.Result[Request[Api]] =
        Decoder.decodeMapLike[HashMap, Symbol, Json].apply(c) match {
          case Left(err) => Left(err)
          case Right(mapOfReqs) =>
            val expectedKeys: Set[Symbol] = keyList.keys.toSet
            val unknownKeys: Set[Symbol] = mapOfReqs.keySet -- expectedKeys
            if (unknownKeys.isEmpty)
              worker.value.apply(mapOfReqs).map(gen.from)
            else
              Left(
                DecodingFailure(
                  s"Saw request with unknown keys: $unknownKeys, expected " +
                    s"keys are: $expectedKeys, object is $mapOfReqs",
                  List()
                ))
        }
    }

  trait Worker[Repr] {
    def apply(o: HashMap[Symbol, Json]): Either[DecodingFailure, Repr]
  }

  object Worker {

    implicit val WorkerHNil: Worker[HNil] = _ => Right(HNil)

    implicit def WorkerAtomE[K <: Symbol, A, E, T <: HList](
      implicit recur: Worker[T],
      kWitness: Witness.Aux[K]
    ): Worker[FieldType[K, Fields.Request#AtomE[E, A]] :: T] =
      (o: HashMap[Symbol, Json]) => {
        val there: Result[T] = recur(o)
        val here: Result[FieldType[K, Fields.Request#AtomE[E, A]]] =
          o.get(kWitness.value) match {
            case None =>
              Right(field[K](false))
            case Some(subJson) =>
              Decoder.decodeBoolean.map(field[K](_)).decodeJson(subJson)
          }
        Apply[Result].map2(here, there)((h, t) => h :: t)
      }

    implicit def WorkerIAtomE[K <: Symbol, I, A, E, T <: HList](
      implicit recur: Worker[T],
      kWitness: Witness.Aux[K],
      iDecoder: KeyDecoder[I]
    ): Worker[FieldType[K, Fields.Request#IAtomE[I, E, A]] :: T] =
      (o: HashMap[Symbol, Json]) => {
        val there: Result[T] = recur(o)
        val here: Result[FieldType[K, Fields.Request#IAtomE[I, E, A]]] =
          o.get(kWitness.value) match {
            case None =>
              Right(field[K](Set[I]()))
            case Some(subJson) =>
              Decoder
                .decodeSet[I](
                  Decoder.decodeString.emap { str =>
                    iDecoder(str) match {
                      case None => Left("bad index: %s" format str)
                      case Some(res) => Right(res)
                    }
                  }
                )
                .map(field[K](_))
                .decodeJson(subJson)
          }
        Apply[Result].map2(here, there)((h, t) => h :: t)
      }

    implicit def WorkerObjE[K <: Symbol,
                            A[_ <: Fields],
                            E,
                            C <: Cardinality,
                            T <: HList](
      implicit recur: Worker[T],
      kWitness: Witness.Aux[K],
      recurA: Lazy[RequestDecoder[A]]
    ): Worker[FieldType[K, Fields.Request#ObjE[C, E, A]] :: T] =
      (o: HashMap[Symbol, Json]) => {
        val later: Result[T] = recur(o)
        val now: Result[FieldType[K, Fields.Request#ObjE[C, E, A]]] =
          o.get(kWitness.value) match {
            case None => Right(field[K](None))
            case Some(subReqJson) =>
              recurA.value
                .decodeJson(subReqJson)
                .map(x => field[K](Some(x)))
          }
        Apply[Result].map2(now, later)((h, t) => h :: t)
      }

    implicit def WorkerIObjE[K <: Symbol,
                             A[_ <: Fields],
                             I,
                             E,
                             C <: Cardinality,
                             T <: HList](
      implicit recur: Worker[T],
      kWitness: Witness.Aux[K],
      recurA: Lazy[RequestDecoder[A]],
      iDecoder: KeyDecoder[I]
    ): Worker[FieldType[K, Fields.Request#IObjE[I, C, E, A]] :: T] =
      (o: HashMap[Symbol, Json]) => {
        val later: Result[T] = recur(o)
        val now: Result[FieldType[K, Fields.Request#IObjE[I, C, E, A]]] =
          o.get(kWitness.value) match {
            case None => Right(field[K](HashMap[I, Request[A]]()))
            case Some(subReqJson) =>
              Decoder
                .decodeMapLike[HashMap, I, Request[A]](iDecoder,
                                                       recurA.value,
                                                       implicitly)
                .decodeJson(subReqJson)
                .map(field[K](_))
          }
        Apply[Result].map2(now, later)((h, t) => h :: t)
      }

  }

}
