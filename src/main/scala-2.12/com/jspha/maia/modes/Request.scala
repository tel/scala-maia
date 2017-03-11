/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.modes

import com.jspha.maia
import scala.collection.immutable.HashMap
import scala.language.higherKinds

trait Request extends maia.Mode {
  type Atom[A] = Boolean
  type IAtom[I, A] = Set[I]
  type Obj[M <: maia.Cardinality, A[_ <: maia.Mode]] = Option[maia.Request[A]]
  type IObj[I, M <: maia.Cardinality, A[_ <: maia.Mode]] =
    HashMap[I, maia.Request[A]]
}

object Request extends Request
