/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.props

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
                                 TI <: HList,
                                 TReq <: HList,
                                 TResp <: HList](
      implicit rWorker: Worker[M, TI, TReq, TResp]
    ): Worker[M,
              FetcherMode[M]#Atom[A] :: TI,
              RequestMode.Atom[A] :: TReq,
              ResponseMode.Atom[A] :: TResp] = {
      (ii: FetcherMode[M]#Atom[A] :: TI, rr: RequestMode.Atom[A] :: TReq) =>
        {
          val M = Monad[M]

          (ii, rr) match {
            case (i :: is, r :: rs) =>
              val here: M[Option[A]] =
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
                                        I,
                                        TI <: HList,
                                        TReq <: HList,
                                        TResp <: HList](
      implicit rWorker: Worker[M, TI, TReq, TResp]
    ): Worker[M,
              FetcherMode[M]#IAtom[I, A] :: TI,
              RequestMode.IAtom[I, A] :: TReq,
              ResponseMode.IAtom[I, A] :: TResp] = {
      (ii: FetcherMode[M]#IAtom[I, A] :: TI,
       rr: RequestMode.IAtom[I, A] :: TReq) =>
        {
          val M = Monad[M]

          (ii, rr) match {
            case (i :: is, r :: rs) =>
              val here: M[HashMap[I, A]] =
                Foldable[Set].foldM(r, HashMap.empty[I, A]) {
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
                             M <: Cardinality,
                             TI <: HList,
                             TReq <: HList,
                             TResp <: HList](
      implicit rWorker: Worker[F, TI, TReq, TResp],
      multOps: Cardinality.Ops[M],
      rObj: Interprets[F, A]
    ): Worker[F,
              FetcherMode[F]#Obj[M, A] :: TI,
              RequestMode.Obj[M, A] :: TReq,
              ResponseMode.Obj[M, A] :: TResp] =
      (ii: FetcherMode[F]#Obj[M, A] :: TI,
       rr: RequestMode.Obj[M, A] :: TReq) => {
        val F = Monad[F]

        (ii, rr) match {
          case (i :: is, r :: rs) =>
            val here: F[Option[M#Coll[Response[A]]]] =
              r match {
                case None => F.pure(None)
                case Some(objReq) =>
                  F.flatMap(i) { (collOfObjInt: M#Coll[Fetcher[F, A]]) =>
                    F.map(
                      multOps.traversable.traverse(collOfObjInt)(
                        rObj(_, objReq))
                    )(Some(_))
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
                                        TI <: HList,
                                        TReq <: HList,
                                        TResp <: HList](
      implicit rWorker: Worker[F, TI, TReq, TResp],
      rObj: Interprets[F, A]
    ): Worker[F,
              FetcherMode[F]#Obj[Cardinality.One, A] :: TI,
              RequestMode.Obj[Cardinality.One, A] :: TReq,
              ResponseMode.Obj[Cardinality.One, A] :: TResp] =
      buildWorkerRecurObjM[F, A, Cardinality.One, TI, TReq, TResp]

    implicit def WorkerRecurObjOptional[F[_]: Monad,
                                        A[_ <: Mode],
                                        TI <: HList,
                                        TReq <: HList,
                                        TResp <: HList](
      implicit rWorker: Worker[F, TI, TReq, TResp],
      rObj: Interprets[F, A]
    ): Worker[F,
              FetcherMode[F]#Obj[Cardinality.Opt, A] :: TI,
              RequestMode.Obj[Cardinality.Opt, A] :: TReq,
              ResponseMode.Obj[Cardinality.Opt, A] :: TResp] =
      buildWorkerRecurObjM[F, A, Cardinality.Opt, TI, TReq, TResp]

    implicit def WorkerRecurObjCollection[F[_]: Monad,
                                          A[_ <: Mode],
                                          TI <: HList,
                                          TReq <: HList,
                                          TResp <: HList](
      implicit rWorker: Worker[F, TI, TReq, TResp],
      rObj: Interprets[F, A]
    ): Worker[F,
              FetcherMode[F]#Obj[Cardinality.Many, A] :: TI,
              RequestMode.Obj[Cardinality.Many, A] :: TReq,
              ResponseMode.Obj[Cardinality.Many, A] :: TResp] =
      buildWorkerRecurObjM[F, A, Cardinality.Many, TI, TReq, TResp]

    def buildWorkerRecurIndexedObjM[F[_]: Monad,
                                    A[_ <: Mode],
                                    I,
                                    M <: Cardinality,
                                    TI <: HList,
                                    TReq <: HList,
                                    TResp <: HList](
      implicit rWorker: Worker[F, TI, TReq, TResp],
      multOps: Cardinality.Ops[M],
      rObj: Interprets[F, A]
    ): Worker[F,
              FetcherMode[F]#IObj[I, M, A] :: TI,
              RequestMode.IObj[I, M, A] :: TReq,
              ResponseMode.IObj[I, M, A] :: TResp] =
      (ii: FetcherMode[F]#IObj[I, M, A] :: TI,
       rr: RequestMode.IObj[I, M, A] :: TReq) => {
        val F = Monad[F]

        (ii, rr) match {
          case (i :: is, r :: rs) =>
            val here: F[HashMap[I, M#Coll[Response[A]]]] =
              Foldable[Set].foldM(r.toSet,
                                  HashMap.empty[I, M#Coll[Response[A]]]) {
                case (map, (ix, subReq)) =>
                  F.flatMap(i(ix)) { fetchers =>
                      multOps.traversable.traverse(fetchers)(rObj(_, subReq))
                    }
                    .map(result => map + (ix -> result))
              }
            val there: F[TResp] = rWorker(is, rs)
            F.map2(here, there)((h, t) => h :: t)
        }
      }

    implicit def WorkerRecurMultiObjSingular[F[_]: Monad,
                                             I,
                                             A[_ <: Mode],
                                             TI <: HList,
                                             TReq <: HList,
                                             TResp <: HList](
      implicit rWorker: Worker[F, TI, TReq, TResp],
      rObj: Interprets[F, A]
    ): Worker[F,
              FetcherMode[F]#IObj[I, Cardinality.One, A] :: TI,
              RequestMode.IObj[I, Cardinality.One, A] :: TReq,
              ResponseMode.IObj[I, Cardinality.One, A] ::
                TResp] =
      buildWorkerRecurIndexedObjM[F, A, I, Cardinality.One, TI, TReq, TResp]

    implicit def WorkerRecurMultiObjOptional[F[_]: Monad,
                                             I,
                                             A[_ <: Mode],
                                             TI <: HList,
                                             TReq <: HList,
                                             TResp <: HList](
      implicit rWorker: Worker[F, TI, TReq, TResp],
      rObj: Interprets[F, A]
    ): Worker[F,
              FetcherMode[F]#IObj[I, Cardinality.Opt, A] :: TI,
              RequestMode.IObj[I, Cardinality.Opt, A] :: TReq,
              ResponseMode.IObj[I, Cardinality.Opt, A] ::
                TResp] =
      buildWorkerRecurIndexedObjM[F, A, I, Cardinality.Opt, TI, TReq, TResp]

    implicit def WorkerRecurMultiObjCollection[F[_]: Monad,
                                               I,
                                               A[_ <: Mode],
                                               TI <: HList,
                                               TReq <: HList,
                                               TResp <: HList](
      implicit rWorker: Worker[F, TI, TReq, TResp],
      rObj: Interprets[F, A]
    ): Worker[F,
              FetcherMode[F]#IObj[I, Cardinality.Many, A] :: TI,
              RequestMode.IObj[I, Cardinality.Many, A] :: TReq,
              ResponseMode.IObj[I, Cardinality.Many, A] ::
                TResp] =
      buildWorkerRecurIndexedObjM[F, A, I, Cardinality.Many, TI, TReq, TResp]

  }

}
