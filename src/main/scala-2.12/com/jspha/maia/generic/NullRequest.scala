/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.generic

import scala.language.higherKinds
import com.jspha.maia._
import shapeless._
import shapeless.labelled._

import scala.collection.immutable.HashMap

trait NullRequest[Api[_ <: Fields]] {
  val request: Request[Api]
}

object NullRequest {

  implicit def NullRequestGeneric[Api[_ <: Fields], Repr <: HList](
    implicit gen: LabelledGeneric.Aux[Request[Api], Repr],
    worker: Worker[Repr]
  ): NullRequest[Api] =
    new NullRequest[Api] {
      val request: Request[Api] = gen.from(worker.request)
    }

  trait Worker[T <: HList] {
    val request: T
  }

  object Worker {

    implicit val WorkerHNil: Worker[HNil] = new Worker[HNil] {
      val request: HNil = HNil
    }

    implicit def WorkerRecurAtom[A, E, K <: Symbol, T <: HList](
      implicit recur: Worker[T]
    ): Worker[FieldType[K, Fields.Request.AtomE[E, A]] :: T] =
      new Worker[FieldType[K, Fields.Request.AtomE[E, A]] :: T] {
        val request: FieldType[K, Fields.Request.AtomE[E, A]] :: T =
          field[K](false) :: recur.request
      }

    implicit def WorkerRecurIndexedAtom[A, E, I, K <: Symbol, T <: HList](
      implicit recur: Worker[T]
    ): Worker[FieldType[K, Fields.Request.IAtomE[I, E, A]] :: T] =
      new Worker[FieldType[K, Fields.Request.IAtomE[I, E, A]] :: T] {
        val request: FieldType[K, Fields.Request.IAtomE[I, E, A]] :: T =
          field[K](Set.empty[I]) :: recur.request
      }

    implicit def WorkerRecurObjM[A[_ <: Fields],
                                 E,
                                 M <: Cardinality,
                                 K <: Symbol,
                                 T <: HList](
      implicit recur: Worker[T]
    ): Worker[FieldType[K, Fields.Request.ObjE[M, E, A]] :: T] =
      new Worker[FieldType[K, Fields.Request.ObjE[M, E, A]] :: T] {
        val request: FieldType[K, Fields.Request.ObjE[M, E, A]] :: T =
          field[K](None) :: recur.request
      }

    implicit def WorkerRecurIndexedMultiObj[A[_ <: Fields],
                                            E,
                                            I,
                                            M <: Cardinality,
                                            K <: Symbol,
                                            T <: HList](
      implicit recur: Worker[T]
    ): Worker[FieldType[K, Fields.Request.IObjE[I, M, E, A]] :: T] =
      new Worker[FieldType[K, Fields.Request.IObjE[I, M, E, A]] :: T] {
        val request: FieldType[K, Fields.Request.IObjE[I, M, E, A]] :: T =
          field[K](HashMap.empty[I, Request[A]]) :: recur.request
      }

  }

}
