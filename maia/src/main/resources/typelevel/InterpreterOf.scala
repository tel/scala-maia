/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.typelevel

import scala.language.higherKinds
import cats._
import com.jspha.maia._
import shapeless._

trait InterpreterOf[F[_], T[_ <: Dsl]] {
  def apply(f: Handler[F, T])(r: Request[T]): F[Response[T]]
}

object InterpreterOf {

  def apply[T[_ <: Dsl]]: PartiallyApplied[T] =
    new PartiallyApplied[T]

  class PartiallyApplied[T[_ <: Dsl]] {
    def apply[F[_]](f: Handler[F, T])(
      implicit interpreterOf: InterpreterOf[F, T])
      : Request[T] => F[Response[T]] = interpreterOf(f)
  }

  implicit def _InterpreterOf[F[_],
                              T[_ <: Dsl],
                              LF <: HList,
                              LReq <: HList,
                              LResp <: HList](
    implicit genericReq: Generic.Aux[Request[T], LReq],
    genericResp: Generic.Aux[Response[T], LResp],
    genericFetcher: Generic.Aux[Handler[F, T], LF],
    worker: Worker[F, LF, LReq, LResp],
    F: Applicative[F]
  ): InterpreterOf[F, T] = {
    new InterpreterOf[F, T] {
      def apply(f: Handler[F, T])(r: Request[T]) = {
        val buildResp: F[LResp] =
          worker(genericFetcher.to(f), genericReq.to(r))
        F.map(buildResp) { (lresp: LResp) =>
          genericResp.from(lresp)
        }
      }
    }
  }

  trait Worker[F[_], LF <: HList, LReq <: HList, LResp <: HList] {
    def apply(f: LF, req: LReq): F[LResp]
  }

  object Worker {

    implicit def _HNil[F[_]](
      implicit F: Applicative[F]): Worker[F, HNil, HNil, HNil] =
      (f: HNil, req: HNil) => F.pure(HNil)

    implicit def _HCons[F[_],
                        HF,
                        HReq,
                        HResp,
                        LF <: HList,
                        LReq <: HList,
                        LResp <: HList](
      implicit recur: Worker[F, LF, LReq, LResp],
      F: Applicative[F],
      headWorker: HeadWorker[F, HF, HReq, HResp]
    ): Worker[F, HF :: LF, HReq :: LReq, HResp :: LResp] =
      (f: HF :: LF, req: HReq :: LReq) =>
        F.map2(headWorker(f.head, req.head), recur(f.tail, req.tail))(_ :: _)

    trait HeadWorker[F[_], HF, HReq, HResp] {
      def apply(f: HF, req: HReq): F[HResp]
    }

    object HeadWorker {
      implicit def _Any[F[_], HF, HReq, HResp]
        : HeadWorker[F, HF, HReq, HResp] = ???
//      implicit def AtomicNANEOne[F[_], A](
//        implicit F: Applicative[F]): HeadWorker[F, F[A], Boolean, Option[A]] =
//        (f: F[A], req: Boolean) => {
//          if (req) F.map(f)(a => Some(a)) else F.pure(None)
//        }

    }

  }

}
