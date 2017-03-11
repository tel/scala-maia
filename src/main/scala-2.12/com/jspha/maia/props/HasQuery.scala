/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.props

import scala.language.higherKinds
import com.jspha.maia._
import shapeless._
import shapeless.labelled._
import shapeless.ops.record._
import cats.data.Validated

import scala.collection.immutable.HashMap

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

        def handleResponse(resp: Response[Api]): Validated[LookupError, A] = {
          Validated.fromOption(
            selector(respRepr.to(resp)),
            LookupError.Unexpected(
              LookupError.UnexpectedError
                .ServerShouldHaveResponded(kWitness.value))
          )
        }

        val lookup: Lookup[Api, A] =
          Lookup[Api, A](request, handleResponse)

        val query: FieldType[K, QueryMode[Api]#Atom[A]] :: T =
          field[K](lookup) :: recur.query

      }

    implicit def WorkerRecurIndexedAtom[K <: Symbol,
                                        ReqRepr <: HList,
                                        RespRepr <: HList,
                                        Api[_ <: Mode],
                                        A,
                                        I,
                                        T <: HList](
      implicit recur: Worker[T],
      kWitness: Witness.Aux[K],
      buildNullRequest: NullRequest[Api],
      reqRepr: LabelledGeneric.Aux[Request[Api], ReqRepr],
      updater: Updater.Aux[ReqRepr,
                           FieldType[K, RequestMode#IAtom[I, A]],
                           ReqRepr],
      respRepr: LabelledGeneric.Aux[Response[Api], RespRepr],
      selector: Selector.Aux[RespRepr, K, ResponseMode#IAtom[I, A]]
    ): Worker[FieldType[K, QueryMode[Api]#IAtom[I, A]] :: T] =
      new Worker[FieldType[K, QueryMode[Api]#IAtom[I, A]] :: T] {

        def lookup(ix: I): Lookup[Api, A] = {

          val request: Request[Api] =
            reqRepr.from(
              updater(reqRepr.to(buildNullRequest.request), field[K](Set(ix)))
            )

          def handleResponse(resp: Response[Api]): Validated[LookupError, A] = {
            Validated.fromOption(
              selector(respRepr.to(resp)).get(ix),
              LookupError.Unexpected(
                LookupError.UnexpectedError
                  .ServerShouldHaveResponded(kWitness.value))
            )
          }

          Lookup[Api, A](request, handleResponse)
        }

        val query: FieldType[K, QueryMode[Api]#IAtom[I, A]] :: T =
          field[K](lookup _) :: recur.query

      }

    implicit def WorkerRecurObjM[K <: Symbol,
                                 ReqRepr <: HList,
                                 RespRepr <: HList,
                                 Api[_ <: Mode],
                                 A[_ <: Mode],
                                 M <: Cardinality,
                                 T <: HList](
      implicit recur: Worker[T],
      kWitness: Witness.Aux[K],
      buildNullRequest: NullRequest[Api],
      reqRepr: LabelledGeneric.Aux[Request[Api], ReqRepr],
      updater: Updater.Aux[ReqRepr,
                           FieldType[K, RequestMode#Obj[M, A]],
                           ReqRepr],
      respRepr: LabelledGeneric.Aux[Response[Api], RespRepr],
      selector: Selector.Aux[RespRepr, K, ResponseMode#Obj[M, A]],
      multOps: Cardinality.Ops[M],
      recurQuery: HasQuery[A]
    ): Worker[FieldType[K, QueryMode[Api]#Obj[M, A]] :: T] =
      new Worker[FieldType[K, QueryMode[Api]#Obj[M, A]] :: T] {

        val qm: QueryMode[Api] = new QueryMode[Api]

        val query: FieldType[K, QueryMode[Api]#Obj[M, A]] :: T = {

          val obj = new qm.Obj[M, A] {
            def apply[R](
              cont: Query[A] => Lookup[A, R]): Lookup[Api, M#Coll[R]] = {

              val subLookup: Lookup[A, R] = cont(recurQuery.query)

              val request: Request[Api] =
                reqRepr.from(
                  updater(
                    reqRepr.to(buildNullRequest.request),
                    field[K](Option(subLookup.request))
                  ))

              def doResp(
                resp: Response[Api]): Validated[LookupError, M#Coll[R]] =
                selector(respRepr.to(resp)) match {
                  case None =>
                    Validated.Invalid(
                      LookupError.Unexpected(LookupError.UnexpectedError
                        .ServerShouldHaveResponded(kWitness.value)))
                  case Some(respA) =>
                    multOps.traversable
                      .traverse[Validated[LookupError, ?], Response[A], R](
                        respA)(subLookup.handleResponse)
                      // We mark the lower errors with an "object group
                      // name" forming a trie of errors
                      .leftMap(LookupError.Object(kWitness.value, _))
                }

              Lookup[Api, M#Coll[R]](request, doResp)
            }
          }

          field[K](obj) :: recur.query
        }
      }

    implicit def WorkerRecurIndexedMultiObj[K <: Symbol,
                                            ReqRepr <: HList,
                                            RespRepr <: HList,
                                            Api[_ <: Mode],
                                            A[_ <: Mode],
                                            I,
                                            M <: Cardinality,
                                            T <: HList](
      implicit recur: Worker[T],
      kWitness: Witness.Aux[K],
      buildNullRequest: NullRequest[Api],
      reqRepr: LabelledGeneric.Aux[Request[Api], ReqRepr],
      updater: Updater.Aux[ReqRepr,
                           FieldType[K, RequestMode#IObj[I, M, A]],
                           ReqRepr],
      respRepr: LabelledGeneric.Aux[Response[Api], RespRepr],
      selector: Selector.Aux[RespRepr, K, ResponseMode#IObj[I, M, A]],
      multOps: Cardinality.Ops[M],
      recurQuery: HasQuery[A]
    ): Worker[FieldType[K, QueryMode[Api]#IObj[I, M, A]] :: T] =
      new Worker[FieldType[K, QueryMode[Api]#IObj[I, M, A]] :: T] {

        val qm: QueryMode[Api] = new QueryMode[Api]

        val query: FieldType[K, QueryMode[Api]#IObj[I, M, A]] :: T = {

          val obj = new qm.IObj[I, M, A] {
            def apply[R](ix: I)(
              cont: Query[A] => Lookup[A, R]): Lookup[Api, M#Coll[R]] = {

              val subLookup: Lookup[A, R] =
                cont(recurQuery.query)

              val request: Request[Api] =
                reqRepr.from(
                  updater(
                    reqRepr.to(buildNullRequest.request),
                    field[K](HashMap(ix -> subLookup.request))
                  ))

              def doResp(
                resp: Response[Api]): Validated[LookupError, M#Coll[R]] =
                selector(respRepr.to(resp)).get(ix) match {
                  case None =>
                    Validated.Invalid(
                      LookupError.Unexpected(LookupError.UnexpectedError
                        .ServerShouldHaveResponded(kWitness.value)))
                  case Some(respA) =>
                    multOps.traversable
                      .traverse[Validated[LookupError, ?], Response[A], R](
                        respA)(subLookup.handleResponse)
                      // We mark the lower errors with an "object group
                      // name" forming a trie of errors
                      .leftMap(LookupError.Object(kWitness.value, _))
                }

              Lookup[Api, M#Coll[R]](request, doResp)
            }
          }

          field[K](obj) :: recur.query
        }
      }

  }

}
