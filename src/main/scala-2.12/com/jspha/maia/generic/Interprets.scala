/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.generic

import scala.language.higherKinds
import shapeless._
import cats._
import cats.implicits._
import com.jspha.maia._

import scala.collection.immutable.HashMap

trait Interprets[M[_], Api[_ <: Mode]] {
  def apply(i: Fetcher[M, Api], r: Request[Api]): M[Response[Api]]
}

object Interprets {

  def apply[F[_], Api[_ <: Mode]](
    implicit I: Interprets[F, Api]): Interprets[F, Api] = I

  implicit def InterpretsGeneric[M[_]: Monad,
                                 Api[_ <: Mode],
                                 ReprI <: HList,
                                 ReprReq <: HList,
                                 ReprResp <: HList](
    implicit geni: Generic.Aux[Fetcher[M, Api], ReprI],
    genReq: Generic.Aux[Request[Api], ReprReq],
    genResp: Generic.Aux[Response[Api], ReprResp],
    worker: Worker[M, ReprI, ReprReq, ReprResp]
  ): Interprets[M, Api] =
    (i: Fetcher[M, Api], r: Request[Api]) =>
      Monad[M].map(worker(geni.to(i), genReq.to(r)))(genResp.from)

  trait Worker[M[_], ReprI <: HList, ReprReq <: HList, ReprResp <: HList] {
    def apply(i: ReprI, r: ReprReq): M[ReprResp]
  }

  object Worker {

    implicit def WorkerHNil[M[_]: Monad]: Worker[M, HNil, HNil, HNil] =
      (i: HNil, r: HNil) => Monad[M].pure(HNil)

    implicit def WorkerRecurAtom[M[_]: Monad,
                                 A,
                                 E,
                                 TI <: HList,
                                 TReq <: HList,
                                 TResp <: HList](
      implicit rWorker: Worker[M, TI, TReq, TResp]
    ): Worker[M,
              Mode.Fetcher[M]#AtomE[E, A] :: TI,
              Mode.Request.AtomE[E, A] :: TReq,
              Mode.Response.AtomE[E, A] :: TResp] = {
      (ii: Mode.Fetcher[M]#AtomE[E, A] :: TI,
       rr: Mode.Request.AtomE[E, A] :: TReq) =>
        {
          val M = Monad[M]

          (ii, rr) match {
            case (i :: is, r :: rs) =>
              val here: M[Option[Either[E, A]]] =
                r match {
                  case false => M.pure(None)
                  case true => M.map(i)(Some(_))
                }
              val there = rWorker(is, rs)
              M.map2(here, there)((h, t) => h :: t)
          }
        }

    }

    implicit def WorkerRecurIndexedAtom[M[_]: Monad,
                                        A,
                                        E,
                                        I,
                                        TI <: HList,
                                        TReq <: HList,
                                        TResp <: HList](
      implicit rWorker: Worker[M, TI, TReq, TResp]
    ): Worker[M,
              Mode.Fetcher[M]#IAtom[I, E, A] :: TI,
              Mode.Request.IAtom[I, E, A] :: TReq,
              Mode.Response.IAtom[I, E, A] :: TResp] = {
      (ii: Mode.Fetcher[M]#IAtom[I, E, A] :: TI,
       rr: Mode.Request.IAtom[I, E, A] :: TReq) =>
        {
          val M = Monad[M]

          (ii, rr) match {
            case (i :: is, r :: rs) =>
              val here: M[HashMap[I, Either[E, A]]] =
                Foldable[Set].foldM(r, HashMap.empty[I, Either[E, A]]) {
                  case (map, ix) =>
                    M.map(i(ix)) { a =>
                      map + (ix -> a)
                    }
                }
              val there = rWorker(is, rs)
              M.map2(here, there)((h, t) => h :: t)
          }
        }

    }

    def buildWorkerRecurObjM[F[_]: Monad,
                             A[_ <: Mode],
                             E,
                             M <: Cardinality,
                             TI <: HList,
                             TReq <: HList,
                             TResp <: HList](
      implicit rWorker: Worker[F, TI, TReq, TResp],
      multOps: Cardinality.Ops[M],
      rObj: Interprets[F, A]
    ): Worker[F,
              Mode.Fetcher[F]#Obj[M, E, A] :: TI,
              Mode.Request.Obj[M, E, A] :: TReq,
              Mode.Response.Obj[M, E, A] :: TResp] =
      (ii: Mode.Fetcher[F]#Obj[M, E, A] :: TI,
       rr: Mode.Request.Obj[M, E, A] :: TReq) => {
        val F = Monad[F]

        (ii, rr) match {
          case (i :: is, r :: rs) =>
            val here: F[Option[Either[E, M#Coll[Response[A]]]]] =
              r match {
                case None => F.pure(None)
                case Some(objReq) =>
                  F.flatMap(i) {
                    case Left(err) =>
                      F.pure(Some(Left(err)))
                    case Right(collOfObjInt) =>
                      F.map(
                        multOps.traversable.traverse(collOfObjInt)(
                          rObj(_, objReq))
                      )(x => Some(Right(x)))
                  }
              }
            val there: F[TResp] = rWorker(is, rs)
            F.map2(here, there)((h, t) => h :: t)
        }
      }

    // NOTE: We need to explicitly introduce implicits for each Multiplicity
    // since implicit resolution cannot "work backwards" from an instance to
    // decide the multiplicity.

    implicit def WorkerRecurObjSingular[F[_]: Monad,
                                        A[_ <: Mode],
                                        E,
                                        TI <: HList,
                                        TReq <: HList,
                                        TResp <: HList](
      implicit rWorker: Worker[F, TI, TReq, TResp],
      rObj: Interprets[F, A]
    ): Worker[F,
              Mode.Fetcher[F]#Obj[Cardinality.One, E, A] :: TI,
              Mode.Request.Obj[Cardinality.One, E, A] :: TReq,
              Mode.Response.Obj[Cardinality.One, E, A] :: TResp] =
      buildWorkerRecurObjM[F, A, E, Cardinality.One, TI, TReq, TResp]

    implicit def WorkerRecurObjOptional[F[_]: Monad,
                                        A[_ <: Mode],
                                        E,
                                        TI <: HList,
                                        TReq <: HList,
                                        TResp <: HList](
      implicit rWorker: Worker[F, TI, TReq, TResp],
      rObj: Interprets[F, A]
    ): Worker[F,
              Mode.Fetcher[F]#Obj[Cardinality.Opt, E, A] :: TI,
              Mode.Request.Obj[Cardinality.Opt, E, A] :: TReq,
              Mode.Response.Obj[Cardinality.Opt, E, A] :: TResp] =
      buildWorkerRecurObjM[F, A, E, Cardinality.Opt, TI, TReq, TResp]

    implicit def WorkerRecurObjCollection[F[_]: Monad,
                                          A[_ <: Mode],
                                          E,
                                          TI <: HList,
                                          TReq <: HList,
                                          TResp <: HList](
      implicit rWorker: Worker[F, TI, TReq, TResp],
      rObj: Interprets[F, A]
    ): Worker[F,
              Mode.Fetcher[F]#Obj[Cardinality.Many, E, A] :: TI,
              Mode.Request.Obj[Cardinality.Many, E, A] :: TReq,
              Mode.Response.Obj[Cardinality.Many, E, A] :: TResp] =
      buildWorkerRecurObjM[F, A, E, Cardinality.Many, TI, TReq, TResp]

    implicit def WorkerRecurObjSingularNothing[F[_]: Monad,
                                               A[_ <: Mode],
                                               TI <: HList,
                                               TReq <: HList,
                                               TResp <: HList](
      implicit rWorker: Worker[F, TI, TReq, TResp],
      rObj: Interprets[F, A]
    ): Worker[F,
              Mode.Fetcher[F]#Obj[Cardinality.One, Nothing, A] :: TI,
              Mode.Request.Obj[Cardinality.One, Nothing, A] :: TReq,
              Mode.Response.Obj[Cardinality.One, Nothing, A] :: TResp] =
      buildWorkerRecurObjM[F, A, Nothing, Cardinality.One, TI, TReq, TResp]

    implicit def WorkerRecurObjOptionalNothing[F[_]: Monad,
                                               A[_ <: Mode],
                                               TI <: HList,
                                               TReq <: HList,
                                               TResp <: HList](
      implicit rWorker: Worker[F, TI, TReq, TResp],
      rObj: Interprets[F, A]
    ): Worker[F,
              Mode.Fetcher[F]#Obj[Cardinality.Opt, Nothing, A] :: TI,
              Mode.Request.Obj[Cardinality.Opt, Nothing, A] :: TReq,
              Mode.Response.Obj[Cardinality.Opt, Nothing, A] :: TResp] =
      buildWorkerRecurObjM[F, A, Nothing, Cardinality.Opt, TI, TReq, TResp]

    implicit def WorkerRecurObjCollectionNothing[F[_]: Monad,
                                                 A[_ <: Mode],
                                                 TI <: HList,
                                                 TReq <: HList,
                                                 TResp <: HList](
      implicit rWorker: Worker[F, TI, TReq, TResp],
      rObj: Interprets[F, A]
    ): Worker[F,
              Mode.Fetcher[F]#Obj[Cardinality.Many, Nothing, A] :: TI,
              Mode.Request.Obj[Cardinality.Many, Nothing, A] :: TReq,
              Mode.Response.Obj[Cardinality.Many, Nothing, A] :: TResp] =
      buildWorkerRecurObjM[F, A, Nothing, Cardinality.Many, TI, TReq, TResp]

    def buildWorkerRecurIndexedObjM[F[_]: Monad,
                                    A[_ <: Mode],
                                    E,
                                    I,
                                    M <: Cardinality,
                                    TI <: HList,
                                    TReq <: HList,
                                    TResp <: HList](
      implicit rWorker: Worker[F, TI, TReq, TResp],
      multOps: Cardinality.Ops[M],
      rObj: Interprets[F, A]
    ): Worker[F,
              Mode.Fetcher[F]#IObj[I, M, E, A] :: TI,
              Mode.Request.IObj[I, M, E, A] :: TReq,
              Mode.Response.IObj[I, M, E, A] :: TResp] =
      (ii: Mode.Fetcher[F]#IObj[I, M, E, A] :: TI,
       rr: Mode.Request.IObj[I, M, E, A] :: TReq) => {
        val F = Monad[F]

        (ii, rr) match {
          case (i :: is, r :: rs) =>
            val here: F[HashMap[I, Either[E, M#Coll[Response[A]]]]] =
              Foldable[Set].foldM(
                r.toSet,
                HashMap.empty[I, Either[E, M#Coll[Response[A]]]]) {
                case (map, (ix, subReq)) =>
                  F.flatMap(i(ix)) {
                    case Left(err) =>
                      F.pure(map + (ix -> Left(err)))
                    case Right(fetchers) =>
                      F.map(
                        multOps.traversable.traverse(fetchers)(rObj(_, subReq))
                      )(x => map + (ix -> Right(x)))
                  }
              }
            val there: F[TResp] = rWorker(is, rs)
            F.map2(here, there)((h, t) => h :: t)
        }
      }

    implicit def WorkerRecurMultiObjSingular[F[_]: Monad,
                                             E,
                                             I,
                                             A[_ <: Mode],
                                             TI <: HList,
                                             TReq <: HList,
                                             TResp <: HList](
      implicit rWorker: Worker[F, TI, TReq, TResp],
      rObj: Interprets[F, A]
    ): Worker[F,
              Mode.Fetcher[F]#IObj[I, Cardinality.One, E, A] :: TI,
              Mode.Request.IObj[I, Cardinality.One, E, A] :: TReq,
              Mode.Response.IObj[I, Cardinality.One, E, A] ::
                TResp] =
      buildWorkerRecurIndexedObjM[F, A, E, I, Cardinality.One, TI, TReq, TResp]

    implicit def WorkerRecurMultiObjOptional[F[_]: Monad,
                                             E,
                                             I,
                                             A[_ <: Mode],
                                             TI <: HList,
                                             TReq <: HList,
                                             TResp <: HList](
      implicit rWorker: Worker[F, TI, TReq, TResp],
      rObj: Interprets[F, A]
    ): Worker[F,
              Mode.Fetcher[F]#IObj[I, Cardinality.Opt, E, A] :: TI,
              Mode.Request.IObj[I, Cardinality.Opt, E, A] :: TReq,
              Mode.Response.IObj[I, Cardinality.Opt, E, A] ::
                TResp] =
      buildWorkerRecurIndexedObjM[F, A, E, I, Cardinality.Opt, TI, TReq, TResp]

    implicit def WorkerRecurMultiObjCollection[F[_]: Monad,
                                               E,
                                               I,
                                               A[_ <: Mode],
                                               TI <: HList,
                                               TReq <: HList,
                                               TResp <: HList](
      implicit rWorker: Worker[F, TI, TReq, TResp],
      rObj: Interprets[F, A]
    ): Worker[F,
              Mode.Fetcher[F]#IObj[I, Cardinality.Many, E, A] :: TI,
              Mode.Request.IObj[I, Cardinality.Many, E, A] :: TReq,
              Mode.Response.IObj[I, Cardinality.Many, E, A] ::
                TResp] =
      buildWorkerRecurIndexedObjM[F,
                                  A,
                                  E,
                                  I,
                                  Cardinality.Many,
                                  TI,
                                  TReq,
                                  TResp]

    implicit def WorkerRecurMultiObjSingularNothing[F[_]: Monad,
                                                    I,
                                                    A[_ <: Mode],
                                                    TI <: HList,
                                                    TReq <: HList,
                                                    TResp <: HList](
      implicit rWorker: Worker[F, TI, TReq, TResp],
      rObj: Interprets[F, A]
    ): Worker[F,
              Mode.Fetcher[F]#IObj[I, Cardinality.One, Nothing, A] :: TI,
              Mode.Request.IObj[I, Cardinality.One, Nothing, A] :: TReq,
              Mode.Response.IObj[I, Cardinality.One, Nothing, A] ::
                TResp] =
      buildWorkerRecurIndexedObjM[F,
                                  A,
                                  Nothing,
                                  I,
                                  Cardinality.One,
                                  TI,
                                  TReq,
                                  TResp]

    implicit def WorkerRecurMultiObjOptionalNothing[F[_]: Monad,
                                                    I,
                                                    A[_ <: Mode],
                                                    TI <: HList,
                                                    TReq <: HList,
                                                    TResp <: HList](
      implicit rWorker: Worker[F, TI, TReq, TResp],
      rObj: Interprets[F, A]
    ): Worker[F,
              Mode.Fetcher[F]#IObj[I, Cardinality.Opt, Nothing, A] :: TI,
              Mode.Request.IObj[I, Cardinality.Opt, Nothing, A] :: TReq,
              Mode.Response.IObj[I, Cardinality.Opt, Nothing, A] ::
                TResp] =
      buildWorkerRecurIndexedObjM[F,
                                  A,
                                  Nothing,
                                  I,
                                  Cardinality.Opt,
                                  TI,
                                  TReq,
                                  TResp]

    implicit def WorkerRecurMultiObjCollectionNothing[F[_]: Monad,
                                                      I,
                                                      A[_ <: Mode],
                                                      TI <: HList,
                                                      TReq <: HList,
                                                      TResp <: HList](
      implicit rWorker: Worker[F, TI, TReq, TResp],
      rObj: Interprets[F, A]
    ): Worker[F,
              Mode.Fetcher[F]#IObj[I, Cardinality.Many, Nothing, A] :: TI,
              Mode.Request.IObj[I, Cardinality.Many, Nothing, A] :: TReq,
              Mode.Response.IObj[I, Cardinality.Many, Nothing, A] ::
                TResp] =
      buildWorkerRecurIndexedObjM[F,
                                  A,
                                  Nothing,
                                  I,
                                  Cardinality.Many,
                                  TI,
                                  TReq,
                                  TResp]

  }

}
