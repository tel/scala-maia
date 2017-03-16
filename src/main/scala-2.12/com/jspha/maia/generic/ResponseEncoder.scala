/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.generic

import scala.language.higherKinds
import io.circe._
import shapeless._
import shapeless.labelled._
import com.jspha.maia._

import scala.collection.immutable.HashMap

trait ResponseEncoder[Api[_ <: Fields]] extends Encoder[Response[Api]]

object ResponseEncoder {

  implicit def ResponseEncoderGeneric[Api[_ <: Fields], Repr <: HList](
    implicit gen: LabelledGeneric.Aux[Response[Api], Repr],
    worker: Lazy[Worker[Repr]]
  ): ResponseEncoder[Api] =
    new ResponseEncoder[Api] {
      def apply(a: Response[Api]): Json =
        Encoder
          .encodeMapLike[HashMap, Symbol, Json]
          .apply(worker.value(gen.to(a)))
    }

  trait Worker[Repr <: HList] {
    def apply(r: Repr): HashMap[Symbol, Json]
  }

  object Worker {

    implicit val WorkerHNil: Worker[HNil] = _ => HashMap()

    implicit val NothingEncoder: Encoder[Nothing] =
      (a: Nothing) => a.asInstanceOf[Json]

    def EAEncoder[E, A](implicit aEncoder: Encoder[A],
                        eEncoder: Encoder[E]): Encoder[Either[E, A]] =
      Encoder.encodeEither("err", "ok")(eEncoder, aEncoder)

    implicit def WorkerAtomE[K <: Symbol, A, E, T <: HList](
      implicit recur: Worker[T],
      aEncoder: Encoder[A],
      eEncoder: Encoder[E],
      kWitness: Witness.Aux[K]
    ): Worker[FieldType[K, Fields.Response#AtomE[E, A]] :: T] =
      (r: FieldType[K, Fields.Response#AtomE[E, A]] :: T) => {
        val later: HashMap[Symbol, Json] = recur(r.tail)
        val head: Option[Either[E, A]] = r.head
        head match {
          case None => later
          case Some(res) =>
            later + (kWitness.value -> EAEncoder[E, A].apply(res))
        }
      }

    /**
      * We need to provide this Nothing-ed instance explicitly in order to
      * relieve the search process from finding Encoder[Nothing].
      */
    implicit def WorkerAtomE_NoError[K <: Symbol, A, T <: HList](
      implicit recur: Worker[T],
      aEncoder: Encoder[A],
      kWitness: Witness.Aux[K]
    ): Worker[FieldType[K, Fields.Response#AtomE[Nothing, A]] :: T] =
      WorkerAtomE[K, A, Nothing, T](recur, aEncoder, NothingEncoder, kWitness)

    implicit def WorkerIAtomE[K <: Symbol, A, I, E, T <: HList](
      implicit recur: Worker[T],
      aEncoder: Encoder[A],
      eEncoder: Encoder[E],
      iEncoder: KeyEncoder[I],
      kWitness: Witness.Aux[K]
    ): Worker[FieldType[K, Fields.Response#IAtomE[I, E, A]] :: T] =
      (r: FieldType[K, Fields.Response#IAtomE[I, E, A]] :: T) => {
        val later: HashMap[Symbol, Json] = recur(r.tail)
        val head: HashMap[I, Either[E, A]] = r.head
        val enc: ObjectEncoder[HashMap[I, Either[E, A]]] =
          Encoder.encodeMapLike[HashMap, I, Either[E, A]](
            iEncoder,
            EAEncoder[E, A]
          )
        val json = enc(head)
        if (head.isEmpty) later else later + (kWitness.value -> json)
      }

    implicit def WorkerIAtomE_NoError[K <: Symbol, A, I, T <: HList](
      implicit recur: Worker[T],
      aEncoder: Encoder[A],
      iEncoder: KeyEncoder[I],
      kWitness: Witness.Aux[K]
    ): Worker[FieldType[K, Fields.Response#IAtomE[I, Nothing, A]] :: T] =
      WorkerIAtomE[K, A, I, Nothing, T](recur,
                                        aEncoder,
                                        NothingEncoder,
                                        iEncoder,
                                        kWitness)

    implicit def WorkerObjE[K <: Symbol,
                            A[_ <: Fields],
                            E,
                            C <: Cardinality,
                            T <: HList](
      implicit recur: Worker[T],
      eEncoder: Encoder[E],
      aEncoder: Lazy[ResponseEncoder[A]],
      collOps: Cardinality.Ops[C],
      kWitness: Witness.Aux[K]
    ): Worker[FieldType[K, Fields.Response#ObjE[C, E, A]] :: T] =
      (r: FieldType[K, Fields.Response#ObjE[C, E, A]] :: T) => {
        val later: HashMap[Symbol, Json] = recur(r.tail)
        val head: Option[Either[E, C#Coll[Response[A]]]] =
          r.head
        head match {
          case None => later
          case Some(subResp) =>
            val json: Json = EAEncoder[E, C#Coll[Response[A]]](
              collOps.encoder(aEncoder.value),
              eEncoder
            ).apply(subResp)
            later + (kWitness.value -> json)
        }
      }

    implicit def WorkerObjE_One[K <: Symbol, A[_ <: Fields], E, T <: HList](
      implicit recur: Worker[T],
      eEncoder: Encoder[E],
      aEncoder: Lazy[ResponseEncoder[A]],
      collOps: Cardinality.Ops[Cardinality.One],
      kWitness: Witness.Aux[K]
    ): Worker[FieldType[K, Fields.Response#ObjE[Cardinality.One, E, A]] :: T] =
      WorkerObjE[K, A, E, Cardinality.One, T](recur,
                                              eEncoder,
                                              aEncoder,
                                              collOps,
                                              kWitness)

    implicit def WorkerObjE_Opt[K <: Symbol, A[_ <: Fields], E, T <: HList](
      implicit recur: Worker[T],
      eEncoder: Encoder[E],
      aEncoder: Lazy[ResponseEncoder[A]],
      collOps: Cardinality.Ops[Cardinality.Opt],
      kWitness: Witness.Aux[K]
    ): Worker[FieldType[K, Fields.Response#ObjE[Cardinality.Opt, E, A]] :: T] =
      WorkerObjE[K, A, E, Cardinality.Opt, T](recur,
                                              eEncoder,
                                              aEncoder,
                                              collOps,
                                              kWitness)

    implicit def WorkerObjE_Many[K <: Symbol, A[_ <: Fields], E, T <: HList](
      implicit recur: Worker[T],
      eEncoder: Encoder[E],
      aEncoder: Lazy[ResponseEncoder[A]],
      collOps: Cardinality.Ops[Cardinality.Many],
      kWitness: Witness.Aux[K]
    ): Worker[FieldType[K, Fields.Response#ObjE[Cardinality.Many, E, A]] :: T] =
      WorkerObjE[K, A, E, Cardinality.Many, T](recur,
                                               eEncoder,
                                               aEncoder,
                                               collOps,
                                               kWitness)

    implicit def WorkerObjE_One_Nothing[K <: Symbol,
                                        A[_ <: Fields],
                                        T <: HList](
      implicit recur: Worker[T],
      aEncoder: Lazy[ResponseEncoder[A]],
      collOps: Cardinality.Ops[Cardinality.One],
      kWitness: Witness.Aux[K]
    ): Worker[FieldType[K, Fields.Response#ObjE[Cardinality.One, Nothing, A]]
      :: T] =
      WorkerObjE[K, A, Nothing, Cardinality.One, T](recur,
                                                    NothingEncoder,
                                                    aEncoder,
                                                    collOps,
                                                    kWitness)

    implicit def WorkerObjE_Opt_Nothing[K <: Symbol,
                                        A[_ <: Fields],
                                        T <: HList](
      implicit recur: Worker[T],
      aEncoder: Lazy[ResponseEncoder[A]],
      collOps: Cardinality.Ops[Cardinality.Opt],
      kWitness: Witness.Aux[K]
    ): Worker[FieldType[K, Fields.Response#ObjE[Cardinality.Opt, Nothing, A]]
      :: T] =
      WorkerObjE[K, A, Nothing, Cardinality.Opt, T](recur,
                                                    NothingEncoder,
                                                    aEncoder,
                                                    collOps,
                                                    kWitness)

    implicit def WorkerObjE_Many_Nothing[K <: Symbol,
                                         A[_ <: Fields],
                                         T <: HList](
      implicit recur: Worker[T],
      aEncoder: Lazy[ResponseEncoder[A]],
      collOps: Cardinality.Ops[Cardinality.Many],
      kWitness: Witness.Aux[K]
    ): Worker[FieldType[K, Fields.Response#ObjE[Cardinality.Many, Nothing, A]]
      :: T] =
      WorkerObjE[K, A, Nothing, Cardinality.Many, T](recur,
                                                     NothingEncoder,
                                                     aEncoder,
                                                     collOps,
                                                     kWitness)

    implicit def WorkerIObjE[K <: Symbol,
                             A[_ <: Fields],
                             I,
                             E,
                             C <: Cardinality,
                             T <: HList](
      implicit recur: Worker[T],
      eEncoder: Encoder[E],
      aEncoder: Lazy[ResponseEncoder[A]],
      iEncoder: KeyEncoder[I],
      collOps: Cardinality.Ops[C],
      kWitness: Witness.Aux[K]
    ): Worker[FieldType[K, Fields.Response#IObjE[I, C, E, A]] :: T] =
      (r: FieldType[K, Fields.Response#IObjE[I, C, E, A]] :: T) => {
        val later: HashMap[Symbol, Json] = recur(r.tail)
        val head: HashMap[I, Either[E, C#Coll[Response[A]]]] =
          r.head
        if (head.isEmpty)
          later
        else {
          val enc =
            Encoder.encodeMapLike[HashMap, I, Either[E, C#Coll[Response[A]]]](
              iEncoder,
              EAEncoder[E, C#Coll[Response[A]]](
                collOps.encoder(aEncoder.value),
                eEncoder
              )
            )
          later + (kWitness.value -> enc.apply(head))
        }
      }

    implicit def WorkerIObjE_One[K <: Symbol,
                                 A[_ <: Fields],
                                 I,
                                 E,
                                 T <: HList](
      implicit recur: Worker[T],
      eEncoder: Encoder[E],
      aEncoder: Lazy[ResponseEncoder[A]],
      collOps: Cardinality.Ops[Cardinality.One],
      iEncoder: KeyEncoder[I],
      kWitness: Witness.Aux[K]
    ): Worker[FieldType[K, Fields.Response#IObjE[I, Cardinality.One, E, A]] ::
      T] =
      WorkerIObjE[K, A, I, E, Cardinality.One, T](recur,
                                                  eEncoder,
                                                  aEncoder,
                                                  iEncoder,
                                                  collOps,
                                                  kWitness)

    implicit def WorkerIObjE_Opt[K <: Symbol,
                                 A[_ <: Fields],
                                 I,
                                 E,
                                 T <: HList](
      implicit recur: Worker[T],
      eEncoder: Encoder[E],
      aEncoder: Lazy[ResponseEncoder[A]],
      iEncoder: KeyEncoder[I],
      collOps: Cardinality.Ops[Cardinality.Opt],
      kWitness: Witness.Aux[K]
    ): Worker[FieldType[K, Fields.Response#IObjE[I, Cardinality.Opt, E, A]] ::
      T] =
      WorkerIObjE[K, A, I, E, Cardinality.Opt, T](recur,
                                                  eEncoder,
                                                  aEncoder,
                                                  iEncoder,
                                                  collOps,
                                                  kWitness)

    implicit def WorkerIObjE_Many[K <: Symbol,
                                  A[_ <: Fields],
                                  I,
                                  E,
                                  T <: HList](
      implicit recur: Worker[T],
      eEncoder: Encoder[E],
      aEncoder: Lazy[ResponseEncoder[A]],
      iEncoder: KeyEncoder[I],
      collOps: Cardinality.Ops[Cardinality.Many],
      kWitness: Witness.Aux[K]
    ): Worker[
      FieldType[K, Fields.Response#IObjE[I, Cardinality.Many, E, A]] :: T] =
      WorkerIObjE[K, A, I, E, Cardinality.Many, T](recur,
                                                   eEncoder,
                                                   aEncoder,
                                                   iEncoder,
                                                   collOps,
                                                   kWitness)

    implicit def WorkerIObjE_One_Nothing[K <: Symbol,
                                         A[_ <: Fields],
                                         I,
                                         T <: HList](
      implicit recur: Worker[T],
      aEncoder: Lazy[ResponseEncoder[A]],
      iEncoder: KeyEncoder[I],
      collOps: Cardinality.Ops[Cardinality.One],
      kWitness: Witness.Aux[K]
    ): Worker[
      FieldType[K, Fields.Response#IObjE[I, Cardinality.One, Nothing, A]]
        :: T] =
      WorkerIObjE[K, A, I, Nothing, Cardinality.One, T](recur,
                                                        NothingEncoder,
                                                        aEncoder,
                                                        iEncoder,
                                                        collOps,
                                                        kWitness)

    implicit def WorkerIObjE_Opt_Nothing[K <: Symbol,
                                         A[_ <: Fields],
                                         I,
                                         T <: HList](
      implicit recur: Worker[T],
      aEncoder: Lazy[ResponseEncoder[A]],
      iEncoder: KeyEncoder[I],
      collOps: Cardinality.Ops[Cardinality.Opt],
      kWitness: Witness.Aux[K]
    ): Worker[
      FieldType[K, Fields.Response#IObjE[I, Cardinality.Opt, Nothing, A]]
        :: T] =
      WorkerIObjE[K, A, I, Nothing, Cardinality.Opt, T](recur,
                                                        NothingEncoder,
                                                        aEncoder,
                                                        iEncoder,
                                                        collOps,
                                                        kWitness)

    implicit def WorkerIObjE_Many_Nothing[K <: Symbol,
                                          A[_ <: Fields],
                                          I,
                                          T <: HList](
      implicit recur: Worker[T],
      aEncoder: Lazy[ResponseEncoder[A]],
      iEncoder: KeyEncoder[I],
      collOps: Cardinality.Ops[Cardinality.Many],
      kWitness: Witness.Aux[K]
    ): Worker[
      FieldType[K, Fields.Response#IObjE[I, Cardinality.Many, Nothing, A]]
        :: T] =
      WorkerIObjE[K, A, I, Nothing, Cardinality.Many, T](recur,
                                                         NothingEncoder,
                                                         aEncoder,
                                                         iEncoder,
                                                         collOps,
                                                         kWitness)

  }

}
