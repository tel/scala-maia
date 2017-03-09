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
              FetcherMode[M]#IndexedAtom[I, A] :: TI,
              RequestMode.IndexedAtom[I, A] :: TReq,
              ResponseMode.IndexedAtom[I, A] :: TResp] = {
      (ii: FetcherMode[M]#IndexedAtom[I, A] :: TI,
       rr: RequestMode.IndexedAtom[I, A] :: TReq) =>
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

    implicit def WorkerRecurObj[F[_]: Monad,
                                A[_ <: Mode],
                                TI <: HList,
                                TReq <: HList,
                                TResp <: HList](
      implicit rWorker: Worker[F, TI, TReq, TResp],
      rObj: Interprets[F, A]
    ): Worker[F,
              FetcherMode[F]#Obj[A] :: TI,
              RequestMode.Obj[A] :: TReq,
              ResponseMode.Obj[A] :: TResp] = {
      (ii: F[Fetcher[F, A]] :: TI, rr: Option[Request[A]] :: TReq) =>
        {
          val F = Monad[F]

          (ii, rr) match {
            case (i :: is, r :: rs) =>
              val here: F[Option[Response[A]]] =
                r match {
                  case None => F.pure(None)
                  case Some(objReq) =>
                    F.flatMap(i) { objInt =>
                      F.map(rObj(objInt, objReq))(Some(_))
                    }
                }
              val there: F[TResp] = rWorker(is, rs)
              F.map2(here, there)((h, t) => h :: t)
          }
        }

    }

    implicit def WorkerRecurObjM[F[_]: Monad,
                                 A[_ <: Mode],
                                 M <: Multiplicity,
                                 TI <: HList,
                                 TReq <: HList,
                                 TResp <: HList](
      implicit rWorker: Worker[F, TI, TReq, TResp],
      multOps: Multiplicity.Ops[M],
      rObj: Interprets[F, A]
    ): Worker[F,
              FetcherMode[F]#ObjM[M, A] :: TI,
              RequestMode.ObjM[M, A] :: TReq,
              ResponseMode.ObjM[M, A] :: TResp] =
      (ii: FetcherMode[F]#ObjM[M, A] :: TI,
       rr: RequestMode.ObjM[M, A] :: TReq) => {
        val F = Monad[F]

        (ii, rr) match {
          case (i :: is, r :: rs) =>
            val here: F[Option[M#Wrap[Response[A]]]] =
              r match {
                case None => F.pure(None)
                case Some(objReq) =>
                  F.flatMap(i) { (collOfObjInt: M#Wrap[Fetcher[F, A]]) =>
                    F.map(
                      multOps.traversable.traverse(collOfObjInt.it)(
                        rObj(_, objReq))
                    )(x => Some(multOps.wrap(x)))
                  }
              }
            val there: F[TResp] = rWorker(is, rs)
            F.map2(here, there)((h, t) => h :: t)
        }
      }

    implicit def WorkerRecurIndexedObj[M[_]: Monad,
                                       A[_ <: Mode],
                                       I,
                                       TI <: HList,
                                       TReq <: HList,
                                       TResp <: HList](
      implicit rWorker: Worker[M, TI, TReq, TResp],
      rObj: Interprets[M, A]
    ): Worker[M,
              FetcherMode[M]#IndexedObj[I, A] :: TI,
              RequestMode.IndexedObj[I, A] :: TReq,
              ResponseMode.IndexedObj[I, A] :: TResp] = {
      (ii: FetcherMode[M]#IndexedObj[I, A] :: TI,
       rr: RequestMode.IndexedObj[I, A] :: TReq) =>
        {
          val M = Monad[M]
          import M._

          (ii, rr) match {
            case (i :: is, r :: rs) =>
              val i_ : I => M[Fetcher[M, A]] = i
              val r_ : HashMap[I, Request[A]] = r
              val here: M[HashMap[I, Response[A]]] =
                Foldable[Set].foldM(r.toSet, HashMap.empty[I, Response[A]]) {
                  case (map, (ix, subReq)) =>
                    M.flatMap(i(ix)) { fetcher =>
                      rObj(fetcher, subReq).map { result =>
                        map + (ix -> result)
                      }
                    }
                }
              val there: M[TResp] = rWorker(is, rs)
              map2(here, there)((h, t) => h :: t)
          }
        }

    }

  }

}
