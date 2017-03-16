/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.generic

import scala.language.higherKinds
import scala.collection.immutable.HashMap
import cats._
import cats.implicits._
import io.circe._
import shapeless._
import shapeless.labelled._
import com.jspha.maia._
import io.circe.Decoder.Result

trait ResponseDecoder[Api[_ <: Fields]] extends Decoder[Response[Api]]

object ResponseDecoder {

  implicit def ResponseDecoderGeneric[Api[_ <: Fields], Repr <: HList](
    implicit gen: LabelledGeneric.Aux[Response[Api], Repr],
    worker: Lazy[Worker[Repr]],
    keyList: KeyList[Repr]
  ): ResponseDecoder[Api] = new ResponseDecoder[Api] {
    def apply(c: HCursor): Decoder.Result[Response[Api]] =
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
                s"Saw response with unknown keys: $unknownKeys, expected " +
                  s"keys are: $expectedKeys, object is $mapOfReqs",
                List()
              ))
      }
  }

  trait Worker[Repr] {
    def apply(o: HashMap[Symbol, Json]): Either[DecodingFailure, Repr]
  }

  object Worker {

    val NothingDecoder: Decoder[Nothing] = new Decoder[Nothing] {
      def apply(c: HCursor): Result[Nothing] =
        Left(
          DecodingFailure(
            "Tried to decode Nothing",
            List()
          ))
    }

    def EADecoder[E, A](implicit aDecoder: Decoder[A],
                        eDecoder: Decoder[E]): Decoder[Either[E, A]] =
      Decoder.decodeEither("err", "ok")(eDecoder, aDecoder)

    implicit val WorkerHNil: Worker[HNil] = _ => Right(HNil)

    implicit def WorkerAtomE[K <: Symbol, A, E, T <: HList](
      implicit recur: Worker[T],
      aDecoder: Decoder[A],
      eDecoder: Decoder[E],
      kWitness: Witness.Aux[K]
    ): Worker[FieldType[K, Fields.Response#AtomE[E, A]] :: T] =
      (o: HashMap[Symbol, Json]) => {
        val there: Result[T] = recur(o)
        val here: Result[FieldType[K, Fields.Response#AtomE[E, A]]] =
          o.get(kWitness.value) match {
            case None =>
              Right(field[K](None))
            case Some(subJson) =>
              EADecoder[E, A](aDecoder, eDecoder)
                .decodeJson(subJson)
                .map { ea =>
                  field[K](Some(ea))
                }
          }
        Apply[Result].map2(here, there)((h, t) => h :: t)
      }

    implicit def WorkerAtomE_Nothing[K <: Symbol, A, T <: HList](
      implicit recur: Worker[T],
      aDecoder: Decoder[A],
      kWitness: Witness.Aux[K]
    ): Worker[FieldType[K, Fields.Response#AtomE[Nothing, A]] :: T] =
      WorkerAtomE[K, A, Nothing, T](recur, aDecoder, NothingDecoder, kWitness)

    implicit def WorkerIAtomE[K <: Symbol, I, A, E, T <: HList](
      implicit recur: Worker[T],
      aDecoder: Decoder[A],
      eDecoder: Decoder[E],
      iDecoder: KeyDecoder[I],
      kWitness: Witness.Aux[K]
    ): Worker[FieldType[K, Fields.Response#IAtomE[I, E, A]] :: T] =
      (o: HashMap[Symbol, Json]) => {
        val there: Result[T] = recur(o)
        val here: Result[FieldType[K, Fields.Response#IAtomE[I, E, A]]] =
          o.get(kWitness.value) match {
            case None =>
              Right(field[K](HashMap[I, Either[E, A]]()))
            case Some(subJson) =>
              Decoder
                .decodeMapLike[HashMap, I, Either[E, A]](
                  iDecoder,
                  EADecoder[E, A](aDecoder, eDecoder),
                  implicitly
                )
                .decodeJson(subJson)
                .map(field[K](_))
          }
        Apply[Result].map2(here, there)((h, t) => h :: t)
      }

    implicit def WorkerIAtomE_Nothing[K <: Symbol, I, A, T <: HList](
      implicit recur: Worker[T],
      aDecoder: Decoder[A],
      iDecoder: KeyDecoder[I],
      kWitness: Witness.Aux[K]
    ): Worker[FieldType[K, Fields.Response#IAtomE[I, Nothing, A]] :: T] =
      WorkerIAtomE[K, I, A, Nothing, T](recur,
                                        aDecoder,
                                        NothingDecoder,
                                        iDecoder,
                                        kWitness)

    implicit def WorkerObjE[K <: Symbol,
                            A[_ <: Fields],
                            E,
                            C <: Cardinality,
                            T <: HList](
      implicit recur: Worker[T],
      kWitness: Witness.Aux[K],
      aDecoder: Lazy[ResponseDecoder[A]],
      eDecoder: Decoder[E],
      cardOps: Cardinality.Ops[C]
    ): Worker[FieldType[K, Fields.Response#ObjE[C, E, A]] :: T] =
      (o: HashMap[Symbol, Json]) => {
        val later = recur(o)
        val now = o.get(kWitness.value) match {
          case None =>
            Right(field[K](None))
          case Some(json) =>
            val d: Decoder[Either[E, C#Coll[Response[A]]]] =
              EADecoder[E, C#Coll[Response[A]]](
                cardOps.decoder(aDecoder.value),
                eDecoder
              )
            d.decodeJson(json)
              .map(x => field[K](Some(x)))
        }
        Apply[Result].map2(now, later)((h, t) => h :: t)
      }

    implicit def WorkerObjE_One[K <: Symbol, A[_ <: Fields], E, T <: HList](
      implicit recur: Worker[T],
      kWitness: Witness.Aux[K],
      aDecoder: Lazy[ResponseDecoder[A]],
      eDecoder: Decoder[E],
      cardOps: Cardinality.Ops[Cardinality.One]
    ): Worker[FieldType[K, Fields.Response#ObjE[Cardinality.One, E, A]] :: T] =
      WorkerObjE[K, A, E, Cardinality.One, T](recur,
                                              kWitness,
                                              aDecoder,
                                              eDecoder,
                                              cardOps)

    implicit def WorkerObjE_Opt[K <: Symbol, A[_ <: Fields], E, T <: HList](
      implicit recur: Worker[T],
      kWitness: Witness.Aux[K],
      aDecoder: Lazy[ResponseDecoder[A]],
      eDecoder: Decoder[E],
      cardOps: Cardinality.Ops[Cardinality.Opt]
    ): Worker[FieldType[K, Fields.Response#ObjE[Cardinality.Opt, E, A]] :: T] =
      WorkerObjE[K, A, E, Cardinality.Opt, T](recur,
                                              kWitness,
                                              aDecoder,
                                              eDecoder,
                                              cardOps)

    implicit def WorkerObjE_Many[K <: Symbol, A[_ <: Fields], E, T <: HList](
      implicit recur: Worker[T],
      kWitness: Witness.Aux[K],
      aDecoder: Lazy[ResponseDecoder[A]],
      eDecoder: Decoder[E],
      cardOps: Cardinality.Ops[Cardinality.Many]
    ): Worker[FieldType[K, Fields.Response#ObjE[Cardinality.Many, E, A]] :: T] =
      WorkerObjE[K, A, E, Cardinality.Many, T](recur,
                                               kWitness,
                                               aDecoder,
                                               eDecoder,
                                               cardOps)

    implicit def WorkerObjE_One_Nothing[K <: Symbol,
                                        A[_ <: Fields],
                                        T <: HList](
      implicit recur: Worker[T],
      kWitness: Witness.Aux[K],
      aDecoder: Lazy[ResponseDecoder[A]],
      cardOps: Cardinality.Ops[Cardinality.One]
    ): Worker[FieldType[K, Fields.Response#ObjE[Cardinality.One, Nothing, A]]
      :: T] =
      WorkerObjE[K, A, Nothing, Cardinality.One, T](recur,
                                                    kWitness,
                                                    aDecoder,
                                                    NothingDecoder,
                                                    cardOps)

    implicit def WorkerObjE_Opt_Nothing[K <: Symbol,
                                        A[_ <: Fields],
                                        T <: HList](
      implicit recur: Worker[T],
      kWitness: Witness.Aux[K],
      aDecoder: Lazy[ResponseDecoder[A]],
      cardOps: Cardinality.Ops[Cardinality.Opt]
    ): Worker[
      FieldType[K, Fields.Response#ObjE[Cardinality.Opt, Nothing, A]] :: T] =
      WorkerObjE[K, A, Nothing, Cardinality.Opt, T](recur,
                                                    kWitness,
                                                    aDecoder,
                                                    NothingDecoder,
                                                    cardOps)

    implicit def WorkerObjE_Many_Nothing[K <: Symbol,
                                         A[_ <: Fields],
                                         T <: HList](
      implicit recur: Worker[T],
      kWitness: Witness.Aux[K],
      aDecoder: Lazy[ResponseDecoder[A]],
      cardOps: Cardinality.Ops[Cardinality.Many]
    ): Worker[
      FieldType[K, Fields.Response#ObjE[Cardinality.Many, Nothing, A]] :: T] =
      WorkerObjE[K, A, Nothing, Cardinality.Many, T](recur,
                                                     kWitness,
                                                     aDecoder,
                                                     NothingDecoder,
                                                     cardOps)

    implicit def WorkerIObjE[K <: Symbol,
                             I,
                             A[_ <: Fields],
                             E,
                             C <: Cardinality,
                             T <: HList](
      implicit recur: Worker[T],
      kWitness: Witness.Aux[K],
      aDecoder: Lazy[ResponseDecoder[A]],
      eDecoder: Decoder[E],
      iDecoder: KeyDecoder[I],
      cardOps: Cardinality.Ops[C]
    ): Worker[FieldType[K, Fields.Response#IObjE[I, C, E, A]] :: T] =
      (o: HashMap[Symbol, Json]) => {
        val later = recur(o)
        val now
          : Result[FieldType[K, HashMap[I, Either[E, C#Coll[Response[A]]]]]] =
          o.get(kWitness.value) match {
            case None =>
              Right(field[K](HashMap[I, Either[E, C#Coll[Response[A]]]]()))
            case Some(json) =>
              val d: Decoder[HashMap[I, Either[E, C#Coll[Response[A]]]]] =
                Decoder
                  .decodeMapLike[HashMap, I, Either[E, C#Coll[Response[A]]]](
                    iDecoder,
                    EADecoder[E, C#Coll[Response[A]]](
                      cardOps.decoder(aDecoder.value),
                      eDecoder
                    ),
                    implicitly
                  )
              d.decodeJson(json).map(field[K](_))
          }
        Apply[Result].map2(now, later)((h, t) => h :: t)
      }

    implicit def WorkerIObjE_One[K <: Symbol,
                                 I,
                                 A[_ <: Fields],
                                 E,
                                 T <: HList](
      implicit recur: Worker[T],
      kWitness: Witness.Aux[K],
      aDecoder: Lazy[ResponseDecoder[A]],
      eDecoder: Decoder[E],
      iDecoder: KeyDecoder[I],
      cardOps: Cardinality.Ops[Cardinality.One]
    ): Worker[
      FieldType[K, Fields.Response#IObjE[I, Cardinality.One, E, A]] :: T] =
      WorkerIObjE[K, I, A, E, Cardinality.One, T](recur,
                                                  kWitness,
                                                  aDecoder,
                                                  eDecoder,
                                                  iDecoder,
                                                  cardOps)

    implicit def WorkerIObjE_Opt[K <: Symbol,
                                 I,
                                 A[_ <: Fields],
                                 E,
                                 T <: HList](
      implicit recur: Worker[T],
      kWitness: Witness.Aux[K],
      aDecoder: Lazy[ResponseDecoder[A]],
      eDecoder: Decoder[E],
      iDecoder: KeyDecoder[I],
      cardOps: Cardinality.Ops[Cardinality.Opt]
    ): Worker[
      FieldType[K, Fields.Response#IObjE[I, Cardinality.Opt, E, A]] :: T] =
      WorkerIObjE[K, I, A, E, Cardinality.Opt, T](recur,
                                                  kWitness,
                                                  aDecoder,
                                                  eDecoder,
                                                  iDecoder,
                                                  cardOps)

    implicit def WorkerIObjE_Many[K <: Symbol,
                                  I,
                                  A[_ <: Fields],
                                  E,
                                  T <: HList](
      implicit recur: Worker[T],
      kWitness: Witness.Aux[K],
      aDecoder: Lazy[ResponseDecoder[A]],
      eDecoder: Decoder[E],
      iDecoder: KeyDecoder[I],
      cardOps: Cardinality.Ops[Cardinality.Many]
    ): Worker[
      FieldType[K, Fields.Response#IObjE[I, Cardinality.Many, E, A]] :: T] =
      WorkerIObjE[K, I, A, E, Cardinality.Many, T](recur,
                                                   kWitness,
                                                   aDecoder,
                                                   eDecoder,
                                                   iDecoder,
                                                   cardOps)

    implicit def WorkerIObjE_One_Nothing[K <: Symbol,
                                         I,
                                         A[_ <: Fields],
                                         T <: HList](
      implicit recur: Worker[T],
      kWitness: Witness.Aux[K],
      aDecoder: Lazy[ResponseDecoder[A]],
      iDecoder: KeyDecoder[I],
      cardOps: Cardinality.Ops[Cardinality.One]
    ): Worker[
      FieldType[K, Fields.Response#IObjE[I, Cardinality.One, Nothing, A]] ::
        T] =
      WorkerIObjE[K, I, A, Nothing, Cardinality.One, T](recur,
                                                        kWitness,
                                                        aDecoder,
                                                        NothingDecoder,
                                                        iDecoder,
                                                        cardOps)

    implicit def WorkerIObjE_Opt_Nothing[K <: Symbol,
                                         I,
                                         A[_ <: Fields],
                                         T <: HList](
      implicit recur: Worker[T],
      kWitness: Witness.Aux[K],
      aDecoder: Lazy[ResponseDecoder[A]],
      iDecoder: KeyDecoder[I],
      cardOps: Cardinality.Ops[Cardinality.Opt]
    ): Worker[
      FieldType[K, Fields.Response#IObjE[I, Cardinality.Opt, Nothing, A]] ::
        T] =
      WorkerIObjE[K, I, A, Nothing, Cardinality.Opt, T](recur,
                                                        kWitness,
                                                        aDecoder,
                                                        NothingDecoder,
                                                        iDecoder,
                                                        cardOps)

    implicit def WorkerIObjE_Many_Nothing[K <: Symbol,
                                          I,
                                          A[_ <: Fields],
                                          T <: HList](
      implicit recur: Worker[T],
      kWitness: Witness.Aux[K],
      aDecoder: Lazy[ResponseDecoder[A]],
      iDecoder: KeyDecoder[I],
      cardOps: Cardinality.Ops[Cardinality.Many]
    ): Worker[
      FieldType[K, Fields.Response#IObjE[I, Cardinality.Many, Nothing, A]] ::
        T] =
      WorkerIObjE[K, I, A, Nothing, Cardinality.Many, T](recur,
                                                         kWitness,
                                                         aDecoder,
                                                         NothingDecoder,
                                                         iDecoder,
                                                         cardOps)

  }

}
