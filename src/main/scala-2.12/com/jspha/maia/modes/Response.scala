/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

package com.jspha.maia.modes

import com.jspha.maia
import scala.collection.immutable.HashMap
import scala.language.higherKinds

trait Response extends maia.Mode {
  type Atom[A] = Option[A]
  type IAtom[I, A] = HashMap[I, A]
  type Obj[M <: maia.Cardinality, A[_ <: maia.Mode]] =
    Option[M#Coll[maia.Response[A]]]
  type IObj[I, M <: maia.Cardinality, A[_ <: maia.Mode]] =
    HashMap[I, M#Coll[maia.Response[A]]]
}

object Response extends Response
