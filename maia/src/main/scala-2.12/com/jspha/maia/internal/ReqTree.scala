/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.internal

import scala.language.higherKinds
import com.jspha.maia._

/**
  * A [[ReqTree]] is a binary, non-empty tree of [[Request]]s.
  *
  * Building [[Query]] values requires a lot of [[Request]] merging. If we
  * merge each [[Request]] pairwise that requires `3n` trips between "flat"
  * and [[shapeless.HList]] representations.
  *
  * Instead, we collect the [[Request]]s in a [[ReqTree]] trading a small
  * amount of memory so as to only need to collapse the [[Request]]s at the
  * end.
  */
sealed trait ReqTree[T[_ <: Dsl]] {

  /**
    * Pass once through the [[ReqTree]] and compute the combined [[Request]].
    */
  def request(implicit merger: typelevel.MergeRequests[T]): Request[T] =
    merger.mergeTree(this)

  def *(other: ReqTree[T]): ReqTree[T] = ReqTree.Branch[T](this, other)
}

object ReqTree {

  def apply[T[_ <: Dsl]](req: Request[T]): ReqTree[T] =
    Leaf[T](req)

  final case class Leaf[T[_ <: Dsl]](leaf: Request[T]) extends ReqTree[T]

  final case class Branch[T[_ <: Dsl]](left: ReqTree[T], right: ReqTree[T])
      extends ReqTree[T]

}
