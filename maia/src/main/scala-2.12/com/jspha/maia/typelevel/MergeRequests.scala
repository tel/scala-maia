/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.typelevel

import com.jspha.maia._
import com.jspha.maia.internal.ReqTree
import shapeless._
import shapeless.labelled._
import shapeless.ops.hlist.ZipWith

import scala.collection.immutable.HashMap
import scala.language.higherKinds

trait MergeRequests[T[_ <: Dsl]] {
  def apply(r1: Request[T], r2: Request[T]): Request[T]
  def mergeTree(reqs: ReqTree[T]): Request[T]
}

object MergeRequests {

  def apply[T[_ <: Dsl]](r1: Request[T], r2: Request[T])(
    implicit merger: Lazy[MergeRequests[T]]): Request[T] =
    merger.value(r1, r2)

  implicit def mergeRequestsG[T[_ <: Dsl], L <: HList](
    implicit generic: Generic.Aux[Request[T], L],
    zipper: ZipWith.Aux[L, L, Merge1.type, L]): MergeRequests[T] =
    new MergeRequests[T] {
      def apply(r1: Request[T], r2: Request[T]) =
        generic.from(zipper(generic.to(r1), generic.to(r2)))

      def go(reqs: ReqTree[T]): L = reqs match {
        case ReqTree.Leaf(leaf) => generic.to(leaf)
        case ReqTree.Branch(l, r) => zipper(go(l), go(r))
      }

      def mergeTree(reqs: ReqTree[T]) =
        generic.from(go(reqs))
    }

  object Merge1 extends Poly2 {

    implicit val Boolean: Case.Aux[Boolean, Boolean, Boolean] =
      at { case (a, b) => a || b }

    implicit def Set[Ag]: Case.Aux[Set[Ag], Set[Ag], Set[Ag]] =
      at { case (a, b) => a union b }

    implicit def Option[S[_ <: Dsl]](implicit merger: Lazy[MergeRequests[S]])
      : Case.Aux[Option[Request[S]], Option[Request[S]], Option[Request[S]]] =
      at {
        case (None, r) => r
        case (l, None) => l
        case (Some(l), Some(r)) =>
          Some(merger.value(l, r))
      }

    implicit def HashMap[Ag, S[_ <: Dsl]](
      implicit merger: Lazy[MergeRequests[S]])
      : Case.Aux[HashMap[Ag, Request[S]],
                 HashMap[Ag, Request[S]],
                 HashMap[Ag, Request[S]]] =
      at {
        case (l, r) =>
          l.merged(r) {
            case ((lk, lv), (_, rv)) =>
              (lk, merger.value(lv, rv))
          }
      }

  }

}
