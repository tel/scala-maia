/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.typelevel

import scala.language.higherKinds
import com.jspha.maia._
import com.jspha.maia.internal.{Field, Fix, ReqTree}
import shapeless._
import shapeless.labelled._
import shapeless.ops.record.{Selector, Updater}
import cats.data._

import scala.collection.immutable.HashMap

trait GetQueriesAt[T[_ <: Dsl]] {
  def apply(): QueriesAt[T]
}

object GetQueriesAt {

  def apply[T[_ <: Dsl]](
    implicit getQueriesAt: Lazy[GetQueriesAt[T]]): QueriesAt[T] =
    getQueriesAt.value()

  implicit def getQueriesAtG[T[_ <: Dsl], L <: HList, Lf <: HList](
    implicit generic: LabelledGeneric.Aux[QueriesAt[T], L],
    genericFx: LabelledGeneric.Aux[T[Fix[form.QueriesAt[T]]], Lf],
    filler: LabelledFiller.Aux[form.QueriesAt[T], FieldSource.type, Lf, L]
  ): GetQueriesAt[T] =
    () => generic.from(filler())

  object FieldSource extends Poly0 {

    implicit def AtomCore[K <: Symbol,
                          T[_ <: Dsl],
                          A,
                          As <: ArgSpec,
                          Es <: ErrSpec,
                          S <: Size,
                          ReqL <: HList,
                          RespL <: HList](
      implicit kWitness: Witness.Aux[K],
      reqGeneric: LabelledGeneric.Aux[Request[T], ReqL],
      respGeneric: LabelledGeneric.Aux[Response[T], RespL],
      reqUpdater: Updater.Aux[ReqL,
                              FieldType[K, form.Request#AtomK[A, As, Es, S]],
                              ReqL],
      respSelector: Selector.Aux[RespL, K, form.Response#AtomK[A, As, Es, S]],
      req0: NullRequest[T],
      atomBuilder: AtomBuilder[T, A, As, Es, S]
    ): Case0[FieldType[K, Field.Atom[A, form.QueriesAt[T], As, Es, S]]] = {

      def projectResp(r: Response[T]): form.Response#AtomK[A, As, Es, S] =
        respSelector(respGeneric.to(r))

      def injectReq(f: form.Request#AtomK[A, As, Es, S]): Request[T] =
        reqGeneric.from(reqUpdater(reqGeneric.to(req0()), field[K](f)))

      at(field[K](atomBuilder(kWitness.value, projectResp, injectReq)))
    }

    trait AtomBuilder[T[_ <: Dsl], A, As <: ArgSpec, Es <: ErrSpec, S <: Size] {
      def apply(name: Symbol,
                projectResp: Response[T] => form.Response#AtomK[A, As, Es, S],
                injectReq: form.Request#AtomK[A, As, Es, S] => Request[T])
        : Field.Atom[A, form.QueriesAt[T], As, Es, S]
    }

    object AtomBuilder {

      implicit def NANE[T[_ <: Dsl], A, S <: Size]
        : AtomBuilder[T, A, NoArg, NoErr, S] =
        (name: Symbol,
         projectResp: Response[T] => Option[S#Coll[A]],
         injectReq: Boolean => Request[T]) =>
          Field.Atom[A, form.QueriesAt[T], NoArg, NoErr, S](
            Query[T, Nothing, S#Coll[A]](
              ReqTree[T](injectReq(true)),
              (resp: Response[T]) => {
                projectResp(resp) match {
                  case None =>
                    Validated.Invalid(
                      Query.Error.Unexpected(Query.Error.UnexpectedError
                        .ServerShouldHaveResponded(name)))
                  case Some(a) =>
                    Validated.Valid(a)
                }
              }
            ))

      implicit def ANE[T[_ <: Dsl], A, Ag, S <: Size]
        : AtomBuilder[T, A, HasArg[Ag], NoErr, S] =
        (name: Symbol,
         projectResp: Response[T] => HashMap[Ag, S#Coll[A]],
         injectReq: Set[Ag] => Request[T]) =>
          Field.Atom[A, form.QueriesAt[T], HasArg[Ag], NoErr, S](
            (ag: Ag) =>
              Query[T, Nothing, S#Coll[A]](
                ReqTree[T](injectReq(Set(ag))),
                (resp: Response[T]) => {
                  projectResp(resp).get(ag) match {
                    case None =>
                      Validated.Invalid(
                        Query.Error.Unexpected(Query.Error.UnexpectedError
                          .ServerShouldHaveResponded(name)))
                    case Some(a) =>
                      Validated.Valid(a)
                  }
                }
            ))

      implicit def NAE[T[_ <: Dsl], A, E, S <: Size]
        : AtomBuilder[T, A, NoArg, HasErr[E], S] =
        (name: Symbol,
         projectResp: Response[T] => Option[Either[E, S#Coll[A]]],
         injectReq: Boolean => Request[T]) =>
          Field.Atom[A, form.QueriesAt[T], NoArg, HasErr[E], S](
            Query[T, E, S#Coll[A]](
              ReqTree[T](injectReq(true)),
              (resp: Response[T]) => {
                projectResp(resp) match {
                  case None =>
                    Validated.Invalid(
                      Query.Error.Unexpected(Query.Error.UnexpectedError
                        .ServerShouldHaveResponded(name)))
                  case Some(Left(e)) =>
                    Validated.Invalid(Query.Error.Domain(e))
                  case Some(Right(a)) =>
                    Validated.Valid(a)
                }
              }
            ))

      implicit def AE[T[_ <: Dsl], A, E, Ag, S <: Size]
        : AtomBuilder[T, A, HasArg[Ag], HasErr[E], S] =
        (name: Symbol,
         projectResp: Response[T] => HashMap[Ag, Either[E, S#Coll[A]]],
         injectReq: Set[Ag] => Request[T]) =>
          Field.Atom[A, form.QueriesAt[T], HasArg[Ag], HasErr[E], S](
            (ag: Ag) =>
              Query[T, E, S#Coll[A]](
                ReqTree[T](injectReq(Set(ag))),
                (resp: Response[T]) => {
                  projectResp(resp).get(ag) match {
                    case None =>
                      Validated.Invalid(
                        Query.Error.Unexpected(Query.Error.UnexpectedError
                          .ServerShouldHaveResponded(name)))
                    case Some(Left(e)) =>
                      Validated.Invalid(Query.Error.Domain(e))
                    case Some(Right(a)) =>
                      Validated.Valid(a)
                  }
                }
            ))

    }

    implicit def ObjCore[K <: Symbol,
                         T[_ <: Dsl],
                         U[_ <: Dsl],
                         As <: ArgSpec,
                         Es <: ErrSpec,
                         S <: Size,
                         ReqL <: HList,
                         RespL <: HList](
      implicit kWitness: Witness.Aux[K],
      reqGeneric: LabelledGeneric.Aux[Request[T], ReqL],
      respGeneric: LabelledGeneric.Aux[Response[T], RespL],
      reqUpdater: Updater.Aux[ReqL,
                              FieldType[K, form.Request#ObjK[U, As, Es, S]],
                              ReqL],
      respSelector: Selector.Aux[RespL, K, form.Response#ObjK[U, As, Es, S]],
      req0: NullRequest[T],
      objBuilder: ObjBuilder[T, U, As, Es, S]
    ): Case0[FieldType[K, Field.Obj[U, form.QueriesAt[T], As, Es, S]]] = {

      def projectResp(r: Response[T]): form.Response#ObjK[U, As, Es, S] =
        respSelector(respGeneric.to(r))

      def injectReq(f: form.Request#ObjK[U, As, Es, S]): Request[T] =
        reqGeneric.from(reqUpdater(reqGeneric.to(req0()), field[K](f)))

      at(field[K](objBuilder(kWitness.value, projectResp, injectReq)))

    }

    trait ObjBuilder[
      T[_ <: Dsl], U[_ <: Dsl], As <: ArgSpec, Es <: ErrSpec, S <: Size] {
      def apply(name: Symbol,
                projectResp: Response[T] => form.Response#ObjK[U, As, Es, S],
                injectReq: form.Request#ObjK[U, As, Es, S] => Request[T])
        : Field.Obj[U, form.QueriesAt[T], As, Es, S]
    }

    object ObjBuilder {

      implicit def NANE[T[_ <: Dsl], U[_ <: Dsl], S <: Size](
        implicit merger: MergeRequests[U],
        collOps: Size.Ops[S]
      ): ObjBuilder[T, U, NoArg, NoErr, S] =
        (name: Symbol,
         projectResp: Response[T] => Option[S#Coll[Response[U]]],
         injectReq: Option[Request[U]] => Request[T]) =>
          Field.Obj[U, form.QueriesAt[T], NoArg, NoErr, S](
            new Query.Transformer[T, U, Nothing, S] {
              def apply[R](subQ: Query[U, Nothing, R]) = {
                Query[T, Nothing, S#Coll[R]](
                  requests = ReqTree[T](injectReq(Some(subQ.request(merger)))),
                  handleResponse = (resp: Response[T]) =>
                    projectResp(resp) match {
                      case None =>
                        Validated.Invalid(
                          Query.Error.Unexpected(Query.Error.UnexpectedError
                            .ServerShouldHaveResponded(name)))
                      case Some(subResp) =>
                        collOps.traversable
                          .traverse[Validated[Query.Error[Nothing], ?],
                                    Response[U],
                                    R](subResp)(subQ.handleResponse)
                  }
                )
              }

            }
        )

      implicit def NAE[T[_ <: Dsl], U[_ <: Dsl], E, S <: Size](
        implicit merger: MergeRequests[U],
        collOps: Size.Ops[S]
      ): ObjBuilder[T, U, NoArg, HasErr[E], S] =
        (name: Symbol,
         projectResp: Response[T] => Option[Either[E, S#Coll[Response[U]]]],
         injectReq: Option[Request[U]] => Request[T]) =>
          Field.Obj[U, form.QueriesAt[T], NoArg, HasErr[E], S](
            new Query.Transformer[T, U, E, S] {
              def apply[R](subQ: Query[U, E, R]) = {
                Query[T, E, S#Coll[R]](
                  requests = ReqTree[T](injectReq(Some(subQ.request(merger)))),
                  handleResponse = (resp: Response[T]) =>
                    projectResp(resp) match {
                      case None =>
                        Validated.Invalid(
                          Query.Error.Unexpected(Query.Error.UnexpectedError
                            .ServerShouldHaveResponded(name)))
                      case Some(Left(e)) =>
                        Validated.Invalid(Query.Error.Domain(e))
                      case Some(Right(subResp)) =>
                        collOps.traversable
                          .traverse[Validated[Query.Error[E], ?],
                                    Response[U],
                                    R](subResp)(subQ.handleResponse)
                  }
                )
              }

            }
        )

      implicit def ANE[T[_ <: Dsl], U[_ <: Dsl], Ag, S <: Size](
        implicit merger: MergeRequests[U],
        collOps: Size.Ops[S]
      ): ObjBuilder[T, U, HasArg[Ag], NoErr, S] =
        (name: Symbol,
         projectResp: Response[T] => HashMap[Ag, S#Coll[Response[U]]],
         injectReq: HashMap[Ag, Request[U]] => Request[T]) =>
          Field.Obj[U, form.QueriesAt[T], HasArg[Ag], NoErr, S](
            (ag: Ag) =>
              new Query.Transformer[T, U, Nothing, S] {
                def apply[R](subQ: Query[U, Nothing, R]) = {
                  Query[T, Nothing, S#Coll[R]](
                    requests = ReqTree[T](
                      injectReq(HashMap(ag -> subQ.request(merger)))),
                    handleResponse = (resp: Response[T]) =>
                      projectResp(resp).get(ag) match {
                        case None =>
                          Validated.Invalid(
                            Query.Error.Unexpected(Query.Error.UnexpectedError
                              .ServerShouldHaveResponded(name)))
                        case Some(subResp) =>
                          collOps.traversable
                            .traverse[Validated[Query.Error[Nothing], ?],
                                      Response[U],
                                      R](subResp)(subQ.handleResponse)
                    }
                  )
                }

            }
        )

      implicit def AE[T[_ <: Dsl], U[_ <: Dsl], Ag, E, S <: Size](
        implicit merger: MergeRequests[U],
        collOps: Size.Ops[S]
      ): ObjBuilder[T, U, HasArg[Ag], HasErr[E], S] =
        (name: Symbol,
         projectResp: Response[T] => HashMap[Ag,
                                             Either[E, S#Coll[Response[U]]]],
         injectReq: HashMap[Ag, Request[U]] => Request[T]) =>
          Field.Obj[U, form.QueriesAt[T], HasArg[Ag], HasErr[E], S](
            (ag: Ag) =>
              new Query.Transformer[T, U, E, S] {
                def apply[R](subQ: Query[U, E, R]) = {
                  Query[T, E, S#Coll[R]](
                    requests = ReqTree[T](
                      injectReq(HashMap(ag -> subQ.request(merger)))),
                    handleResponse = (resp: Response[T]) =>
                      projectResp(resp).get(ag) match {
                        case None =>
                          Validated.Invalid(
                            Query.Error.Unexpected(Query.Error.UnexpectedError
                              .ServerShouldHaveResponded(name)))
                        case Some(Left(e)) =>
                          Validated.Invalid(Query.Error.Domain(e))
                        case Some(Right(subResp)) =>
                          collOps.traversable
                            .traverse[Validated[Query.Error[E], ?],
                                      Response[U],
                                      R](subResp)(subQ.handleResponse)
                    }
                  )
                }

            }
        )

    }

  }

}
