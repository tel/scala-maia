/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia

import scala.collection.immutable.HashMap
import scala.language.higherKinds

object ResponseMode extends Mode {
  type Atom[A] = Option[A]
  type IndexedAtom[I, A] = HashMap[I, A]
  type Obj[A[_ <: Mode]] = Option[Response[A]]
  type ObjM[M <: Multiplicity, A[_ <: Mode]] = Option[M#Wrap[Response[A]]]
  type IndexedObj[I, A[_ <: Mode]] = HashMap[I, Response[A]]
}
