/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import scala.collection.immutable.HashMap
import scala.language.higherKinds

object RequestMode extends Mode {
  type Atom[A] = Boolean
  type IAtom[I, A] = Set[I]
  type Obj[M <: Cardinality, A[_ <: Mode]] = Option[Request[A]]
  type IObj[I, M <: Cardinality, A[_ <: Mode]] =
    HashMap[I, Request[A]]
}
