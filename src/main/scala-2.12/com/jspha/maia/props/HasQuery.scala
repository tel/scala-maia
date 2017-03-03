/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.props

import cats.data.Validated

import scala.language.higherKinds
import com.jspha.maia._
import shapeless._
import shapeless.labelled._
import shapeless.ops.record._
import shapeless.record._

trait HasQuery[A[_ <: Mode]] {
  val query: Query[A]
}

object HasQuery {

  implicit def HasQueryGeneric[Api[_ <: Mode], Repr <: HList](
    implicit gen: LabelledGeneric.Aux[Query[Api], Repr],
    worker: Worker[Repr]
  ): HasQuery[Api] =
    new HasQuery[Api] {
      val query: Query[Api] = gen.from(worker.query)
    }

  trait Worker[T <: HList] {
    val query: T
  }

  object Worker {

    implicit val WorkerHNil: Worker[HNil] =
      new Worker[HNil] {
        val query: HNil = HNil
      }

    implicit def WorkerRecurAtom[K <: Symbol,
                                 ReqRepr <: HList,
                                 RespRepr <: HList,
                                 Api[_ <: Mode],
                                 A,
                                 T <: HList](
      implicit recur: Worker[T],
      kWitness: Witness.Aux[K],
      buildNullRequest: NullRequest[Api],
      reqRepr: LabelledGeneric.Aux[Request[Api], ReqRepr],
      updater: Updater.Aux[ReqRepr,
                           FieldType[K, RequestMode#Atom[A]],
                           ReqRepr],
      respRepr: LabelledGeneric.Aux[Response[Api], RespRepr],
      selector: Selector.Aux[RespRepr, K, ResponseMode#Atom[A]]
    ): Worker[FieldType[K, QueryMode[Api]#Atom[A]] :: T] =
      new Worker[FieldType[K, QueryMode[Api]#Atom[A]] :: T] {

        val request: Request[Api] =
          reqRepr.from(
            updater(reqRepr.to(buildNullRequest.request), field[K](true))
          )

        def handleResponse(resp: Response[Api]): Validated[LookupError, A] =
          Validated.fromOption(
            selector(respRepr.to(resp)),
            LookupError.ResponseMissingCRITICAL(kWitness.value)
          )

        val lookup: Lookup[Api, A] =
          Lookup[Api, A](request, handleResponse)

        val query: FieldType[K, QueryMode[Api]#Atom[A]] :: T =
          field[K](lookup) :: recur.query

      }

    implicit def WorkerRecurObj[K <: Symbol,
                                ReqRepr <: HList,
                                RespRepr <: HList,
                                Api[_ <: Mode],
                                A[_ <: Mode],
                                T <: HList](
      implicit recur: Worker[T],
      kWitness: Witness.Aux[K],
      buildNullRequest: NullRequest[Api],
      reqRepr: LabelledGeneric.Aux[Request[Api], ReqRepr],
      updater: Updater.Aux[ReqRepr, FieldType[K, RequestMode#Obj[A]], ReqRepr],
      respRepr: LabelledGeneric.Aux[Response[Api], RespRepr],
      selector: Selector.Aux[RespRepr, K, ResponseMode#Obj[A]],
      recurQuery: HasQuery[A]
    ): Worker[FieldType[K, QueryMode[Api]#Obj[A]] :: T] =
      new Worker[FieldType[K, QueryMode[Api]#Obj[A]] :: T] {

        val qm: QueryMode[Api] = new QueryMode[Api]

        val query: FieldType[K, QueryMode[Api]#Obj[A]] :: T = {

          val obj = new qm.Obj[A] {
            def apply[R](cont: Query[A] => Lookup[A, R]): Lookup[Api, R] = {

              val subLookup: Lookup[A, R] = cont(recurQuery.query)

              val request: Request[Api] =
                reqRepr.from(
                  updater(
                    reqRepr.to(buildNullRequest.request),
                    field[K](Option(subLookup.request))
                  ))

              def doResp(resp: Response[Api]): Validated[LookupError, R] =
                selector(respRepr.to(resp)) match {
                  case None =>
                    Validated.Invalid(
                      LookupError.ResponseMissingCRITICAL(kWitness.value))
                  case Some(respA) =>
                    // We mark the lower errors with an "object group
                    // name" forming a trie of errors
                    subLookup
                      .handleResponse(respA)
                      .leftMap(LookupError.Nested(kWitness.value, _))
                }

              Lookup[Api, R](request, doResp)
            }
          }

          field[K](obj) :: recur.query
        }
      }

  }

}
