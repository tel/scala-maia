/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.generic

import scala.language.higherKinds
import com.jspha.maia._
import shapeless._

import scala.collection.immutable.HashMap

// TODO: Implement this computation via implicits
trait MergeRequests[Api[_ <: Fields]] {
  def apply(a: Request[Api], b: Request[Api]): Request[Api]
}

object MergeRequests {

  implicit def MergeRequestsGeneric[Api[_ <: Fields], Repr <: HList](
    implicit gen: Generic.Aux[Request[Api], Repr],
    worker: Worker[Repr]
  ): MergeRequests[Api] =
    (a: Request[Api], b: Request[Api]) =>
      gen.from(worker(gen.to(a), gen.to(b)))

  trait Worker[Repr <: HList] {
    def apply(l: Repr, r: Repr): Repr
  }

  object Worker {

    implicit val WorkerHNil: Worker[HNil] = (l: HNil, r: HNil) => HNil

    implicit def WorkerRecurAtom[A, E, Tail <: HList](
      implicit recur: Worker[Tail]
    ): Worker[Fields.Request.AtomE[E, A] :: Tail] =
      (ll: Fields.Request.AtomE[E, A] :: Tail,
       rr: Fields.Request.AtomE[E, A] :: Tail) =>
        (ll, rr) match {
          case (l :: ls, r :: rs) => (l || r) :: recur(ls, rs)
      }

    implicit def WorkerRecurIndexedAtom[A, E, I, Tail <: HList](
      implicit recur: Worker[Tail]
    ): Worker[Fields.Request.IAtomE[I, E, A] :: Tail] =
      (ll: Fields.Request.IAtomE[I, E, A] :: Tail,
       rr: Fields.Request.IAtomE[I, E, A] :: Tail) =>
        (ll, rr) match {
          case (l :: ls, r :: rs) => (l ++ r) :: recur(ls, rs)
      }

    implicit def WorkerRecurObjM[A[_ <: Fields],
                                 E,
                                 M <: Cardinality,
                                 Tail <: HList](
      implicit recur: Worker[Tail],
      recurObj: MergeRequests[A]
    ): Worker[Fields.Request.ObjE[M, E, A] :: Tail] =
      (ll: Option[Request[A]] :: Tail, rr: Option[Request[A]] :: Tail) =>
        (ll, rr) match {
          case (None :: ls, None :: rs) => None :: recur(ls, rs)
          case (Some(l) :: ls, None :: rs) => Some(l) :: recur(ls, rs)
          case (None :: ls, Some(r) :: rs) => Some(r) :: recur(ls, rs)
          case (Some(l) :: ls, Some(r) :: rs) =>
            Some(recurObj(l, r)) :: recur(ls, rs)
      }

    implicit def WorkerRecurIndexedMultiObj[A[_ <: Fields],
                                            E,
                                            I,
                                            M <: Cardinality,
                                            Tail <: HList](
      implicit recur: Worker[Tail],
      recurObj: MergeRequests[A]
    ): Worker[Fields.Request.IObjE[I, M, E, A] :: Tail] =
      (ll: Fields.Request.IObjE[I, M, E, A] :: Tail,
       rr: Fields.Request.IObjE[I, M, E, A] :: Tail) =>
        (ll, rr) match {
          case (l :: ls, r :: rs) =>
            val here: HashMap[I, Request[A]] = l.merged(r) { (lt, rt) =>
              (lt._1, recurObj(lt._2, rt._2))
            }
            val there: Tail = recur(ls, rs)
            here :: there
      }

  }

}
