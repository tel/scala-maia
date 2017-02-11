/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.props

import scala.language.higherKinds
import cats._
import com.jspha.maia._
import shapeless._

trait FetcherOfConst[M[_], Api[_ <: Mode]] {
  def apply(c: Constant[Api]): Fetcher[M, Api]
}

object FetcherOfConst {

  implicit def FetcherOfConstGeneric[M[_],
                                     Api[_ <: Mode],
                                     ReprK <: HList,
                                     ReprF <: HList](
    implicit genK: Generic.Aux[Constant[Api], ReprK],
    genF: Generic.Aux[Fetcher[M, Api], ReprF],
    worker: Worker[M, ReprK, ReprF]
  ): FetcherOfConst[M, Api] =
    (c: Constant[Api]) => genF.from(worker(genK.to(c)))

  trait Worker[M[_], ReprK <: HList, ReprF <: HList] {
    def apply(c: ReprK): ReprF
  }

  object Worker {

    implicit def WorkerHNil[M[_]]: Worker[M, HNil, HNil] =
      (_: HNil) => HNil

    implicit def WorkerRecurAtom[M[_]: Monad,
                                 A,
                                 TailK <: HList,
                                 TailF <: HList](
      implicit recur: Worker[M, TailK, TailF]
    ): Worker[M,
              ConstantMode.Atom[A] :: TailK,
              FetcherMode[M]#Atom[A] :: TailF] =
      (cc: A :: TailK) => {
        cc match {
          case c :: cs =>
            Monad[M].pure[A](c) :: recur(cs)
        }
      }

    implicit def WorkerRecurObj[M[_]: Monad,
                                A[_ <: Mode],
                                TailK <: HList,
                                TailF <: HList](
      implicit recur: Worker[M, TailK, TailF],
      recurObj: FetcherOfConst[M, A]
    ): Worker[M, ConstantMode.Obj[A] :: TailK, FetcherMode[M]#Obj[A] :: TailF] =
      (cc: Constant[A] :: TailK) =>
        cc match {
          case c :: cs =>
            Monad[M].pure(recurObj(c)) :: recur(cs)
      }
  }

}
