/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.props

import scala.language.higherKinds
import shapeless._
import cats._
import com.jspha.maia._

trait Interprets[M[_], Api[_ <: Mode]] {
  def apply(i: Fetcher[M, Api], r: Request[Api]): M[Response[Api]]
}

object Interprets {

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
      (ii: M[A] :: TI, rr: Boolean :: TReq) =>
        {
          val M = Monad[M]
          import M._

          (ii, rr) match {
            case (i :: is, r :: rs) =>
              val here: M[Option[A]] = if (r) map(i)(Some(_)) else pure(None)
              val there = rWorker(is, rs)
              map2(here, there)((h, t) => h :: t)
          }
        }

    }

    implicit def WorkerRecurObj[M[_]: Monad,
                                A[_ <: Mode],
                                TI <: HList,
                                TReq <: HList,
                                TResp <: HList](
      implicit rWorker: Worker[M, TI, TReq, TResp],
      rObj: Interprets[M, A]
    ): Worker[M,
              FetcherMode[M]#Obj[A] :: TI,
              RequestMode.Obj[A] :: TReq,
              ResponseMode.Obj[A] :: TResp] = {
      (ii: M[Fetcher[M, A]] :: TI, rr: Option[Request[A]] :: TReq) =>
        {
          val M = Monad[M]
          import M._

          (ii, rr) match {
            case (i :: is, r :: rs) =>
              val here: M[Option[Response[A]]] =
                r match {
                  case None => M.pure(None)
                  case Some(objReq) =>
                    M.flatMap(i) { objInt =>
                      M.map(rObj(objInt, objReq))(Some(_))
                    }
                }
              val there: M[TResp] = rWorker(is, rs)
              map2(here, there)((h, t) => h :: t)
          }
        }

    }

  }

}
