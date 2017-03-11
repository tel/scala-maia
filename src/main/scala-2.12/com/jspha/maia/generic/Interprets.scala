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

trait Interprets[M[_], E, Api[_ <: Mode]] {
  def apply(i: Fetcher[M, E, Api], r: Request[Api]): M[Response[E, Api]]
}

object Interprets {

  def apply[F[_], E, Api[_ <: Mode]](
    implicit I: Interprets[F, E, Api]): Interprets[F, E, Api] = I

  implicit def InterpretsGeneric[M[_]: Monad,
                                 E,
                                 Api[_ <: Mode],
                                 ReprI <: HList,
                                 ReprReq <: HList,
                                 ReprResp <: HList](
    implicit geni: Generic.Aux[Fetcher[M, E, Api], ReprI],
    genReq: Generic.Aux[Request[Api], ReprReq],
    genResp: Generic.Aux[Response[E, Api], ReprResp],
    worker: Worker[M, ReprI, ReprReq, ReprResp]
  ): Interprets[M, E, Api] =
    (i: Fetcher[M, E, Api], r: Request[Api]) =>
      Monad[M].map(worker(geni.to(i), genReq.to(r)))(genResp.from)

  trait Worker[M[_], ReprI <: HList, ReprReq <: HList, ReprResp <: HList] {
    def apply(i: ReprI, r: ReprReq): M[ReprResp]
  }

  object Worker {

    implicit def WorkerHNil[M[_]: Monad]: Worker[M, HNil, HNil, HNil] =
      (i: HNil, r: HNil) => Monad[M].pure(HNil)

    implicit def WorkerRecurAtom[M[_]: Monad,
                                 E,
                                 A,
                                 TI <: HList,
                                 TReq <: HList,
                                 TResp <: HList](
      implicit rWorker: Worker[M, TI, TReq, TResp]
    ): Worker[M,
              Mode.Fetcher[M, E]#Atom[A] :: TI,
              Mode.Request.Atom[A] :: TReq,
              Mode.Response[E]#Atom[A] :: TResp] = {
      (ii: Mode.Fetcher[M, E]#Atom[A] :: TI,
       rr: Mode.Request.Atom[A] :: TReq) =>
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
              Mode.Fetcher[M, E]#IAtom[I, A] :: TI,
              Mode.Request.IAtom[I, A] :: TReq,
              Mode.Response[E]#IAtom[I, A] :: TResp] = {
      (ii: Mode.Fetcher[M, E]#IAtom[I, A] :: TI,
       rr: Mode.Request.IAtom[I, A] :: TReq) =>
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
      rObj: Interprets[F, E, A]
    ): Worker[F,
              Mode.Fetcher[F, E]#Obj[M, A] :: TI,
              Mode.Request.Obj[M, A] :: TReq,
              Mode.Response[E]#Obj[M, A] :: TResp] =
      (ii: Mode.Fetcher[F, E]#Obj[M, A] :: TI,
       rr: Mode.Request.Obj[M, A] :: TReq) => {
        val F = Monad[F]

        (ii, rr) match {
          case (i :: is, r :: rs) =>
            val here: F[Option[Either[E, M#Coll[Response[E, A]]]]] =
              r match {
                case None => F.pure(None)
                case Some(objReq) =>
                  F.flatMap(i) {
                    (errOrFetchers: Either[E, M#Coll[Fetcher[F, E, A]]]) =>
                      errOrFetchers match {
                        case Right(collOfObjInt) =>
                          F.map(
                            multOps.traversable.traverse(collOfObjInt)(
                              rObj(_, objReq))
                          )(x => Some(Right(x)))
                        case Left(err) =>
                          F.pure(Some(Left(err)))
                      }
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
      rObj: Interprets[F, E, A]
    ): Worker[F,
              Mode.Fetcher[F, E]#Obj[Cardinality.One, A] :: TI,
              Mode.Request.Obj[Cardinality.One, A] :: TReq,
              Mode.Response[E]#Obj[Cardinality.One, A] :: TResp] =
      buildWorkerRecurObjM[F, A, E, Cardinality.One, TI, TReq, TResp]

    implicit def WorkerRecurObjOptional[F[_]: Monad,
                                        A[_ <: Mode],
                                        E,
                                        TI <: HList,
                                        TReq <: HList,
                                        TResp <: HList](
      implicit rWorker: Worker[F, TI, TReq, TResp],
      rObj: Interprets[F, E, A]
    ): Worker[F,
              Mode.Fetcher[F, E]#Obj[Cardinality.Opt, A] :: TI,
              Mode.Request.Obj[Cardinality.Opt, A] :: TReq,
              Mode.Response[E]#Obj[Cardinality.Opt, A] :: TResp] =
      buildWorkerRecurObjM[F, A, E, Cardinality.Opt, TI, TReq, TResp]

    implicit def WorkerRecurObjCollection[F[_]: Monad,
                                          A[_ <: Mode],
                                          E,
                                          TI <: HList,
                                          TReq <: HList,
                                          TResp <: HList](
      implicit rWorker: Worker[F, TI, TReq, TResp],
      rObj: Interprets[F, E, A]
    ): Worker[F,
              Mode.Fetcher[F, E]#Obj[Cardinality.Many, A] :: TI,
              Mode.Request.Obj[Cardinality.Many, A] :: TReq,
              Mode.Response[E]#Obj[Cardinality.Many, A] :: TResp] =
      buildWorkerRecurObjM[F, A, E, Cardinality.Many, TI, TReq, TResp]

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
      rObj: Interprets[F, E, A]
    ): Worker[F,
              Mode.Fetcher[F, E]#IObj[I, M, A] :: TI,
              Mode.Request.IObj[I, M, A] :: TReq,
              Mode.Response[E]#IObj[I, M, A] :: TResp] =
      (ii: Mode.Fetcher[F, E]#IObj[I, M, A] :: TI,
       rr: Mode.Request.IObj[I, M, A] :: TReq) => {
        val F = Monad[F]

        (ii, rr) match {
          case (i :: is, r :: rs) =>
            val here: F[HashMap[I, Either[E, M#Coll[Response[E, A]]]]] =
              Foldable[Set].foldM(
                r.toSet,
                HashMap.empty[I, Either[E, M#Coll[Response[E, A]]]]) {
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
                                             I,
                                             E,
                                             A[_ <: Mode],
                                             TI <: HList,
                                             TReq <: HList,
                                             TResp <: HList](
      implicit rWorker: Worker[F, TI, TReq, TResp],
      rObj: Interprets[F, E, A]
    ): Worker[F,
              Mode.Fetcher[F, E]#IObj[I, Cardinality.One, A] :: TI,
              Mode.Request.IObj[I, Cardinality.One, A] :: TReq,
              Mode.Response[E]#IObj[I, Cardinality.One, A] ::
                TResp] =
      buildWorkerRecurIndexedObjM[F, A, E, I, Cardinality.One, TI, TReq, TResp]

    implicit def WorkerRecurMultiObjOptional[F[_]: Monad,
                                             I,
                                             E,
                                             A[_ <: Mode],
                                             TI <: HList,
                                             TReq <: HList,
                                             TResp <: HList](
      implicit rWorker: Worker[F, TI, TReq, TResp],
      rObj: Interprets[F, E, A]
    ): Worker[F,
              Mode.Fetcher[F, E]#IObj[I, Cardinality.Opt, A] :: TI,
              Mode.Request.IObj[I, Cardinality.Opt, A] :: TReq,
              Mode.Response[E]#IObj[I, Cardinality.Opt, A] ::
                TResp] =
      buildWorkerRecurIndexedObjM[F, A, E, I, Cardinality.Opt, TI, TReq, TResp]

    implicit def WorkerRecurMultiObjCollection[F[_]: Monad,
                                               I,
                                               E,
                                               A[_ <: Mode],
                                               TI <: HList,
                                               TReq <: HList,
                                               TResp <: HList](
      implicit rWorker: Worker[F, TI, TReq, TResp],
      rObj: Interprets[F, E, A]
    ): Worker[F,
              Mode.Fetcher[F, E]#IObj[I, Cardinality.Many, A] :: TI,
              Mode.Request.IObj[I, Cardinality.Many, A] :: TReq,
              Mode.Response[E]#IObj[I, Cardinality.Many, A] ::
                TResp] =
      buildWorkerRecurIndexedObjM[F,
                                  A,
                                  E,
                                  I,
                                  Cardinality.Many,
                                  TI,
                                  TReq,
                                  TResp]

  }

}
