/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.generic

import shapeless._
import shapeless.labelled._

/**
  * This is obviously redundant, but I couldn't get shapeless.ops.hlist.Keys
  * to work right...!
  */
trait KeyList[L <: HList] {
  val keys: List[Symbol]
}

object KeyList {

  implicit val KeyListHNil: KeyList[HNil] = new KeyList[HNil] {
    val keys: List[Symbol] = List()
  }

  implicit def KeyListHCons[K <: Symbol, V, T <: HList](
    implicit recur: KeyList[T],
    kWitness: Witness.Aux[K]
  ): KeyList[FieldType[K, V] ::
    T] = new KeyList[FieldType[K, V] :: T] {
    val keys: List[Symbol] = kWitness.value :: recur.keys
  }

}
