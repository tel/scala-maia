/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.generic

import scala.language.higherKinds
import com.jspha.maia._
import shapeless._
import shapeless.labelled._
import shapeless.ops.record._
import cats.data.Validated

import scala.collection.immutable.HashMap

trait HasQuery[E, A[_ <: Mode]] {
  val query: Query[E, A]
}

object HasQuery {

  implicit def HasQueryGeneric[E, Api[_ <: Mode], Repr <: HList](
    implicit gen: LabelledGeneric.Aux[Query[E, Api], Repr],
    worker: Worker[Repr]
  ): HasQuery[E, Api] =
    new HasQuery[E, Api] {
      val query: Query[E, Api] = gen.from(worker.query)
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
                                 E,
                                 T <: HList](
      implicit recur: Lazy[Worker[T]],
      kWitness: Witness.Aux[K],
      buildNullRequest: NullRequest[Api],
      reqRepr: LabelledGeneric.Aux[Request[Api], ReqRepr],
      updater: Updater.Aux[ReqRepr,
                           FieldType[K, Mode.Request#Atom[A]],
                           ReqRepr],
      respRepr: LabelledGeneric.Aux[Response[E, Api], RespRepr],
      selector: Selector.Aux[RespRepr, K, Mode.Response[E]#Atom[A]]
    ): Worker[FieldType[K, Mode.Query[Api, E]#Atom[A]] :: T] =
      new Worker[FieldType[K, Mode.Query[Api, E]#Atom[A]] :: T] {

        val request: Request[Api] =
          reqRepr.from(
            updater(reqRepr.to(buildNullRequest.request), field[K](true))
          )

        def handleResponse(
          resp: Response[E, Api]): Validated[LookupError[E], A] = {
          selector(respRepr.to(resp)) match {
            case None =>
              Validated.invalid(
                LookupError.Unexpected(LookupError.UnexpectedError
                  .ServerShouldHaveResponded(kWitness.value)))
            case Some(Left(err)) =>
              Validated.invalid(LookupError.Domain(err))
            case Some(Right(a)) =>
              Validated.valid(a)
          }
        }

        val lookup: Lookup[Api, E, A] =
          Lookup[Api, E, A](request, handleResponse)

        val query: FieldType[K, Mode.Query[Api, E]#Atom[A]] :: T =
          field[K](lookup) :: recur.value.query

      }

    implicit def WorkerRecurIndexedAtom[K <: Symbol,
                                        ReqRepr <: HList,
                                        RespRepr <: HList,
                                        Api[_ <: Mode],
                                        A,
                                        E,
                                        I,
                                        T <: HList](
      implicit recur: Lazy[Worker[T]],
      kWitness: Witness.Aux[K],
      buildNullRequest: NullRequest[Api],
      reqRepr: LabelledGeneric.Aux[Request[Api], ReqRepr],
      updater: Updater.Aux[ReqRepr,
                           FieldType[K, Mode.Request#IAtom[I, A]],
                           ReqRepr],
      respRepr: LabelledGeneric.Aux[Response[E, Api], RespRepr],
      selector: Selector.Aux[RespRepr, K, Mode.Response[E]#IAtom[I, A]]
    ): Worker[FieldType[K, Mode.Query[Api, E]#IAtom[I, A]] :: T] =
      new Worker[FieldType[K, Mode.Query[Api, E]#IAtom[I, A]] :: T] {

        def lookup(ix: I): Lookup[Api, E, A] = {

          val request: Request[Api] =
            reqRepr.from(
              updater(reqRepr.to(buildNullRequest.request), field[K](Set(ix)))
            )

          def handleResponse(
            resp: Response[E, Api]): Validated[LookupError[E], A] = {
            selector(respRepr.to(resp)).get(ix) match {
              case None =>
                Validated.invalid(
                  LookupError.Unexpected(LookupError.UnexpectedError
                    .ServerShouldHaveResponded(kWitness.value)))
              case Some(Left(err)) =>
                Validated.invalid(LookupError.Domain(err))
              case Some(Right(a)) =>
                Validated.valid(a)
            }
          }

          Lookup[Api, E, A](request, handleResponse)
        }

        val query: FieldType[K, Mode.Query[Api, E]#IAtom[I, A]] :: T =
          field[K](lookup _) :: recur.value.query

      }

    implicit def WorkerRecurObjM[K <: Symbol,
                                 ReqRepr <: HList,
                                 RespRepr <: HList,
                                 Api[_ <: Mode],
                                 A[_ <: Mode],
                                 E,
                                 M <: Cardinality,
                                 T <: HList](
      implicit recur: Lazy[Worker[T]],
      kWitness: Witness.Aux[K],
      buildNullRequest: NullRequest[Api],
      reqRepr: LabelledGeneric.Aux[Request[Api], ReqRepr],
      updater: Updater.Aux[ReqRepr,
                           FieldType[K, Mode.Request#Obj[M, A]],
                           ReqRepr],
      respRepr: LabelledGeneric.Aux[Response[E, Api], RespRepr],
      selector: Selector.Aux[RespRepr, K, Mode.Response[E]#Obj[M, A]],
      multOps: Cardinality.Ops[M],
      recurQuery: Lazy[HasQuery[E, A]]
    ): Worker[FieldType[K, Mode.Query[Api, E]#Obj[M, A]] :: T] =
      new Worker[FieldType[K, Mode.Query[Api, E]#Obj[M, A]] :: T] {

        val qm: Mode.Query[Api, E] = new Mode.Query[Api, E]

        val query: FieldType[K, Mode.Query[Api, E]#Obj[M, A]] :: T = {

          val obj = new qm.Obj[M, A] {

            def apply[R](cont: Query[E, A] => Lookup[A, E, R])
              : Lookup[Api, E, M#Coll[R]] = {

              val subLookup: Lookup[A, E, R] = cont(recurQuery.value.query)

              val request: Request[Api] =
                reqRepr.from(
                  updater(
                    reqRepr.to(buildNullRequest.request),
                    field[K](Option(subLookup.request))
                  ))

              def doResp(
                resp: Response[E, Api]): Validated[LookupError[E], M#Coll[R]] =
                selector(respRepr.to(resp)) match {
                  case None =>
                    Validated.Invalid(
                      LookupError.Unexpected(LookupError.UnexpectedError
                        .ServerShouldHaveResponded(kWitness.value)))
                  case Some(Left(err)) =>
                    Validated.Invalid(LookupError.Domain(err))
                  case Some(Right(respA)) =>
                    multOps.traversable
                      .traverse[Validated[LookupError[E], ?],
                                Response[E, A],
                                R](respA)(subLookup.handleResponse)
                      // We mark the lower errors with an "object group
                      // name" forming a trie of errors
                      .leftMap(LookupError.Object(kWitness.value, _))
                }

              Lookup[Api, E, M#Coll[R]](request, doResp)
            }
          }

          field[K](obj) :: recur.value.query
        }
      }

    implicit def WorkerRecurIndexedMultiObj[K <: Symbol,
                                            ReqRepr <: HList,
                                            RespRepr <: HList,
                                            Api[_ <: Mode],
                                            A[_ <: Mode],
                                            E,
                                            I,
                                            M <: Cardinality,
                                            T <: HList](
      implicit recur: Lazy[Worker[T]],
      kWitness: Witness.Aux[K],
      buildNullRequest: NullRequest[Api],
      reqRepr: LabelledGeneric.Aux[Request[Api], ReqRepr],
      updater: Updater.Aux[ReqRepr,
                           FieldType[K, Mode.Request#IObj[I, M, A]],
                           ReqRepr],
      respRepr: LabelledGeneric.Aux[Response[E, Api], RespRepr],
      selector: Selector.Aux[RespRepr, K, Mode.Response[E]#IObj[I, M, A]],
      multOps: Cardinality.Ops[M],
      recurQuery: Lazy[HasQuery[E, A]]
    ): Worker[FieldType[K, Mode.Query[Api, E]#IObj[I, M, A]] :: T] =
      new Worker[FieldType[K, Mode.Query[Api, E]#IObj[I, M, A]] :: T] {

        val qm: Mode.Query[Api, E] = new Mode.Query[Api, E]

        val query: FieldType[K, Mode.Query[Api, E]#IObj[I, M, A]] :: T = {

          val obj = new qm.IObj[I, M, A] {

            def apply[R](ix: I)(cont: Query[E, A] => Lookup[A, E, R])
              : Lookup[Api, E, M#Coll[R]] = {

              val subLookup: Lookup[A, E, R] =
                cont(recurQuery.value.query)

              val request: Request[Api] =
                reqRepr.from(
                  updater(
                    reqRepr.to(buildNullRequest.request),
                    field[K](HashMap(ix -> subLookup.request))
                  ))

              def doResp(
                resp: Response[E, Api]): Validated[LookupError[E], M#Coll[R]] =
                selector(respRepr.to(resp)).get(ix) match {
                  case None =>
                    Validated.Invalid(
                      LookupError.Unexpected(LookupError.UnexpectedError
                        .ServerShouldHaveResponded(kWitness.value)))
                  case Some(Left(err)) =>
                    Validated.Invalid(LookupError.Domain(err))
                  case Some(Right(respA)) =>
                    multOps.traversable
                      .traverse[Validated[LookupError[E], ?],
                                Response[E, A],
                                R](respA)(subLookup.handleResponse)
                      // We mark the lower errors with an "object group
                      // name" forming a trie of errors
                      .leftMap(LookupError.Object(kWitness.value, _))
                }

              Lookup[Api, E, M#Coll[R]](request, doResp)
            }
          }

          field[K](obj) :: recur.value.query
        }
      }

  }

}
